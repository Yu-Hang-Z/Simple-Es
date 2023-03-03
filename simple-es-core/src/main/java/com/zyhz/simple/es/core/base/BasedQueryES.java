package com.zyhz.simple.es.core.base;



import com.alibaba.fastjson2.JSON;
import com.zyhz.simple.es.common.utils.ReflectUtils;
import com.zyhz.simple.es.common.utils.model.BasedQueryCondition;
import com.zyhz.simple.es.core.conditions.EsBasedQuery;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.aggregations.Aggregation;
import org.elasticsearch.search.aggregations.AggregationBuilder;
import org.elasticsearch.search.aggregations.AggregationBuilders;
import org.elasticsearch.search.aggregations.Aggregations;
import org.elasticsearch.search.aggregations.bucket.terms.Terms;
import org.elasticsearch.search.aggregations.bucket.terms.TermsAggregationBuilder;
import org.elasticsearch.search.aggregations.metrics.SumAggregationBuilder;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;

/**
 * @author by zhangyuhang
 */
@Service
public class BasedQueryES<T> {

    @Resource
    private RestHighLevelClient restHighLevelClient;


    public static final String bucketPrefix = "level";

    private Map<String,Object> basedQueryContext;


    private void initContext(EsBasedQuery request){
        basedQueryContext = new HashMap<>();
        basedQueryContext.put("bucketFields", request.getBucketFields());
        basedQueryContext.put("sumFields",  request.getSumFields());
        basedQueryContext.put("level3Polymerization", request.getLevel3Polymerization());
        basedQueryContext.put("fetchSource", request.getFetchSource());
        basedQueryContext.put("clz", request.getClz());
    }


    public List<T> aggregationQuery(EsBasedQuery request) throws IOException {
        // todo 通过参数控制年月索引
        SearchRequest searchRequest = new SearchRequest(request.getIndex());
        //构建查询
        SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();
        BoolQueryBuilder queryBuilder = creatQuery(request.getBasedQueryConditions());
        sourceBuilder.query(queryBuilder);
        sourceBuilder = createCitySearchSourceBuilder(sourceBuilder, request.getBucketFields(), request.getSumFields());
        searchRequest.source(sourceBuilder);
        SearchResponse searchResponse = restHighLevelClient.search(searchRequest, RequestOptions.DEFAULT);
        initContext(request);
        List<T> esBasedQueryDataList = analysisBuckets(searchResponse);
        return esBasedQueryDataList;
    }



    public List baseQuery(EsBasedQuery request) throws IOException {
        // 设置索引
        SearchRequest searchRequest = new SearchRequest(request.getIndex());
        // 构建查询
        SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();
        // 构建返回source
        String[] source = (String[]) request.getFetchSource().toArray(new String[request.getFetchSource().size()]);
        sourceBuilder.fetchSource(source,null);
        BoolQueryBuilder queryBuilder = creatQuery(request.getBasedQueryConditions());
        sourceBuilder.query(queryBuilder);
        sourceBuilder.from(request.getFrom());
        sourceBuilder.size(request.getSize());

        searchRequest.source(sourceBuilder);
        SearchResponse searchResponse = restHighLevelClient.search(searchRequest, RequestOptions.DEFAULT);
        //解析数据
        initContext(request);
        List<T> list = getSC(searchResponse);
        return list;
    }


    private BoolQueryBuilder creatQuery(List<BasedQueryCondition> conditions){
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        for (BasedQueryCondition condition : conditions){
            if (BasedQueryCondition.ConditionType.EQUALS.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.filter(QueryBuilders.termsQuery(condition.getField(), values));
            }
            if (BasedQueryCondition.ConditionType.IN.getType().equals( condition.getConditionType().getType())){
                String values = (String) condition.getValue();
                Object[] arr = Arrays.stream(values.split(",")).toArray();
                queryBuilder.filter(QueryBuilders.termsQuery(condition.getField(), arr));
            }
            if (BasedQueryCondition.ConditionType.FROM_TO.getType().equals( condition.getConditionType().getType())){
                Map<String, Object> rangeMap = (Map<String, Object>) condition.getValue();
                queryBuilder.filter(QueryBuilders.
                        rangeQuery(condition.getField())
                        .from(rangeMap.get("from"))
                        .to(rangeMap.get("to")));
            }
            if (BasedQueryCondition.ConditionType.NOT_EQUALS.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.mustNot(QueryBuilders.termsQuery(condition.getField(), values));
            }
            
        }
        return queryBuilder;

    }



    private SearchSourceBuilder createCitySearchSourceBuilder(SearchSourceBuilder sourceBuilder, Map<String, String> pailMap, Map<String, String> sumMap) {
        sourceBuilder.aggregation(createTermsAggregationBuilder(pailMap, sumMap));
        sourceBuilder.size(0);
        return sourceBuilder;
    }

    /**
     * 动态分桶查询构建
     *
     * @param pailMap 分桶顺序需要固定
     * @return
     */
    private AggregationBuilder createTermsAggregationBuilder(Map<String, String> pailMap, Map<String, String> sumMap) {
        AggregationBuilder childTermAgg = createSumAggregationBuilder(sumMap);
        for (int i = pailMap.size(); i > 0; i--) {
            String key = bucketPrefix + i;
            TermsAggregationBuilder termAgg = AggregationBuilders.terms(key).field(pailMap.get(key));
            if (childTermAgg != null) {
                termAgg.subAggregation(childTermAgg);
            }
            childTermAgg = termAgg.size(1000);
        }
        return childTermAgg;
    }


    /**
     * 动态构建分组后的求和查询条件
     *
     * @param sumMap
     * @return
     */
    private AggregationBuilder createSumAggregationBuilder(Map<String, String> sumMap) {
        AggregationBuilder childSumAgg = null;
        for (Map.Entry<String, String> entry : sumMap.entrySet()) {
            SumAggregationBuilder sumAgg = AggregationBuilders.sum(entry.getKey()).field(entry.getValue());
            if (childSumAgg != null) {
                sumAgg.subAggregation(childSumAgg);
            }
            childSumAgg = sumAgg;
        }
        return childSumAgg;
    }

    private List<T> getSC(SearchResponse sr) {
        List<T> list = new ArrayList<T>();
        Class<T> clz = (Class<T>) basedQueryContext.get("clz");
        List<String> fetchSource = (List<String>) basedQueryContext.get("fetchSource");
        for (SearchHit hit : sr.getHits()) {
            Map<String, Object> source = hit.getSourceAsMap();
            T instanceOfT = this.getInstanceOfT(clz);
            if (!source.isEmpty()) {
                for (Iterator<Map.Entry<String, Object>> it = source.entrySet().iterator(); it.hasNext(); ) {
                    Map.Entry<String, Object> entry = it.next();
                    for (String field : fetchSource){
                        if (field.equals(entry.getKey())) {
                            String fieldType = ReflectUtils.getFieldType(instanceOfT, entry.getKey());
                            reflectionToFillInData(fieldType, instanceOfT, entry.getKey(), entry.getValue());
                        }
                    }
                }
            }
            list.add(instanceOfT);
        }
        return list;
    }

    private void reflectionToFillInData(String fieldType, T instanceOfT, String fieldName ,Object value){
        if ("java.lang.String".equals(fieldType)){
            ReflectUtils.setFieldValue(instanceOfT, fieldName, String.valueOf(value));
        }
        if ("java.math.BigDecimal".equals(fieldType)){
            ReflectUtils.setFieldValue(instanceOfT, fieldName, BigDecimal.valueOf((Double) value));
        }

    }


    /**
     * 解析聚合信息
     *
     * @param searchResponse
     * @return
     */
    private List<T> analysisBuckets(SearchResponse searchResponse) {
        List<T> esBasedQueryDataList = new ArrayList<>();
        String key = bucketPrefix + 1;
        if (searchResponse.getAggregations().get(key) != null) {
            Terms aggMap = searchResponse.getAggregations().get(key);
            List<T> inventoryAggDtoList = analysisLevelBuckets(aggMap,1);
            esBasedQueryDataList.addAll(inventoryAggDtoList);
        }
        return esBasedQueryDataList;
    }


    /**
     * 递归动态解析聚合信息
     */
    private List<T> analysisLevelBuckets(Terms terms, int recursionNumber) {
        List<T> analysisLevelList = new ArrayList<>();
        Map<String, String> inventoryInfo = (Map<String, String>) basedQueryContext.get("inventoryInfo");
        Map<String, String> level3Polymerization = (Map<String, String>) basedQueryContext.get("level3Polymerization");
        if (inventoryInfo == null){
            inventoryInfo = new HashMap<>();
        }

        for (Terms.Bucket bucket : terms.getBuckets()) {
            Aggregations aggregations = bucket.getAggregations();
            String childKey = bucketPrefix + (recursionNumber + 1);
            Terms childAgg = aggregations.get(childKey);
            // 当前层为最后一层
            if (childAgg == null){
                List<T> esBasedQueryData = analysisLastBuckets(terms, recursionNumber);
                analysisLevelList.addAll(esBasedQueryData);
                return esBasedQueryData;
            }
            String date = bucket.getKeyAsString();
            String key = bucketPrefix + recursionNumber;
            inventoryInfo.put(level3Polymerization.get(key), date);
            basedQueryContext.put("inventoryInfo", inventoryInfo);
            List<T> analysisLevel3List = analysisLevelBuckets(childAgg, recursionNumber + 1);
            analysisLevelList.addAll(analysisLevel3List);

        }
        return analysisLevelList;
    }

    /**
     * 解析最后一层桶的聚合数据
     * @param terms
     * @param recursionNumber
     * @return
     */
    private List<T> analysisLastBuckets(Terms terms, int recursionNumber) {
        List<T> analysisLastList = new ArrayList<>();
        Map<String, String> inventoryInfo = (Map<String, String>) basedQueryContext.get("inventoryInfo");
        Map<String, String> level3Polymerization = (Map<String, String>) basedQueryContext.get("level3Polymerization");
        Map<String, String> sumFields = (Map<String, String>) basedQueryContext.get("sumFields");
        for (Terms.Bucket bucket : terms.getBuckets()) {
            String type = bucket.getKeyAsString();
            String key = bucketPrefix + recursionNumber;
            inventoryInfo.put(level3Polymerization.get(key), type);
            Map<String, Aggregation> asMap = bucket.getAggregations().getAsMap();

            Class<T> clz = (Class<T>) basedQueryContext.get("clz");
            T instanceOfT = this.getInstanceOfT(clz);

            //聚合分桶字段
            for (Map.Entry<String, String> entry : inventoryInfo.entrySet()) {
                ReflectUtils.setFieldValue(instanceOfT, entry.getKey(), entry.getValue());
            }
            //聚合计算字段字段
            for (Map.Entry<String, String> entry : sumFields.entrySet()) {
                BigDecimal value = (BigDecimal) JSON.parseObject(JSON.toJSONString(asMap.get(entry.getValue()))).get("value");
                ReflectUtils.setFieldValue(instanceOfT, entry.getKey(), value);
            }
            analysisLastList.add(instanceOfT);
        }
        return analysisLastList;
    }

    private T getInstanceOfT(Class<T> clz){
        T t = null;
        try {
            t = clz.newInstance();
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        return t;
    }







}
