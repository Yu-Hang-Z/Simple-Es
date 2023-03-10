package com.zyhz.simple.es.core.base;



import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.zyhz.simple.es.common.model.BasedCalculationCondition;
import com.zyhz.simple.es.common.utils.ReflectUtils;
import com.zyhz.simple.es.common.enums.ConditionType;
import com.zyhz.simple.es.common.model.BasedQueryCondition;
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
import org.springframework.stereotype.Repository;

import javax.annotation.Resource;
import java.io.IOException;

import java.util.*;

/**
 * @author by zhangyuhang
 */
@Repository
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

    public List<T> query(EsBasedQuery query) throws IOException {

        if (query.getLevel3Polymerization() != null && !query.getLevel3Polymerization().isEmpty()) {
            return aggregationQuery(query);
        } else {
            return baseQuery(query);
        }
    }


    private List<T> aggregationQuery(EsBasedQuery request) throws IOException {
        SearchRequest searchRequest = new SearchRequest(request.getIndex());
        //????????????
        SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();
        BoolQueryBuilder queryBuilder = creatQuery(request.getBasedQueryConditions());
        sourceBuilder.query(queryBuilder);
        sourceBuilder = createCitySearchSourceBuilder(sourceBuilder, request.getBucketFields(), request.getAggregateCalculationConditions());
        searchRequest.source(sourceBuilder);
        SearchResponse searchResponse = restHighLevelClient.search(searchRequest, RequestOptions.DEFAULT);
        initContext(request);
        List<T> esBasedQueryDataList = analysisBuckets(searchResponse);
        return esBasedQueryDataList;
    }



    private List baseQuery(EsBasedQuery request) throws IOException {
        // ????????????
        SearchRequest searchRequest = new SearchRequest(request.getIndex());
        // ????????????
        SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();
        // ????????????source
        String[] source = (String[]) request.getFetchSource().toArray(new String[request.getFetchSource().size()]);
        sourceBuilder.fetchSource(source,null);
        BoolQueryBuilder queryBuilder = creatQuery(request.getBasedQueryConditions());
        sourceBuilder.query(queryBuilder);
        sourceBuilder.from(request.getFrom());
        sourceBuilder.size(request.getSize());

        searchRequest.source(sourceBuilder);
        SearchResponse searchResponse = restHighLevelClient.search(searchRequest, RequestOptions.DEFAULT);
        //????????????
        initContext(request);
        List<T> list = getSC(searchResponse);
        return list;
    }


    private BoolQueryBuilder creatQuery(List<BasedQueryCondition> conditions){
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        if (conditions == null || conditions.size() == 0){
            return queryBuilder;
        }
        for (BasedQueryCondition condition : conditions){
            if (ConditionType.EQUALS.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.filter(QueryBuilders.termsQuery(condition.getField(), values));
            }
            if (ConditionType.NOT_EQUALS.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.mustNot(QueryBuilders.termsQuery(condition.getField(), values));
            }
            if (ConditionType.IN.getType().equals( condition.getConditionType().getType())){
                String values = (String) condition.getValue();
                Object[] arr = Arrays.stream(values.split(",")).toArray();
                queryBuilder.filter(QueryBuilders.termsQuery(condition.getField(), arr));
            }
            if (ConditionType.NOT_IN.getType().equals( condition.getConditionType().getType())){
                String values = (String) condition.getValue();
                Object[] arr = Arrays.stream(values.split(",")).toArray();
                queryBuilder.mustNot(QueryBuilders.termsQuery(condition.getField(), arr));
            }
            if (ConditionType.BETWEEN.getType().equals( condition.getConditionType().getType())){
                Map<String, Object> rangeMap = (Map<String, Object>) condition.getValue();
                queryBuilder.filter(QueryBuilders.
                        rangeQuery(condition.getField())
                        .from(rangeMap.get("from"))
                        .to(rangeMap.get("to")));
            }
            if (ConditionType.NOT_BETWEEN.getType().equals( condition.getConditionType().getType())){
                Map<String, Object> rangeMap = (Map<String, Object>) condition.getValue();
                queryBuilder.mustNot(QueryBuilders.
                        rangeQuery(condition.getField())
                        .from(rangeMap.get("from"))
                        .to(rangeMap.get("to")));
            }
            if (ConditionType.GT.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.filter(QueryBuilders.
                        rangeQuery(condition.getField())
                        .gt(values));
            }
            if (ConditionType.GTE.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.filter(QueryBuilders.
                        rangeQuery(condition.getField())
                        .gte(values));
            }
            if (ConditionType.LT.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.filter(QueryBuilders.
                        rangeQuery(condition.getField())
                        .lt(values));
            }
            if (ConditionType.LTE.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.filter(QueryBuilders.
                        rangeQuery(condition.getField())
                        .lte(values));
            }
            if (ConditionType.LIKE.getType().equals( condition.getConditionType().getType())){
                String values = (String) condition.getValue();
                queryBuilder.filter(QueryBuilders.
                        wildcardQuery(condition.getField(), values));
            }
            if (ConditionType.NOT_LIKE.getType().equals( condition.getConditionType().getType())){
                String values = (String) condition.getValue();
                queryBuilder.mustNot(QueryBuilders.
                        wildcardQuery(condition.getField(), values));
            }
            if (ConditionType.IS_NULL.getType().equals( condition.getConditionType().getType())){
                queryBuilder.filter(QueryBuilders.
                        boolQuery()
                        .mustNot(QueryBuilders.existsQuery(condition.getField())));
            }
            if (ConditionType.IS_NOT_NULL.getType().equals( condition.getConditionType().getType())){
                queryBuilder.filter(QueryBuilders.
                        boolQuery()
                        .must(QueryBuilders.existsQuery(condition.getField())));
            }
        }
        return queryBuilder;

    }



    private SearchSourceBuilder createCitySearchSourceBuilder(SearchSourceBuilder sourceBuilder, Map<String, String> pailMap, List<BasedCalculationCondition> conditions) {
        TermsAggregationBuilder termsAgg = createTermsAggregationBuilder(pailMap, conditions);
        sourceBuilder.aggregation(termsAgg);
        sourceBuilder.size(0);
        return sourceBuilder;
    }

    /**
     * ????????????????????????
     * @param pailMap ????????????????????????
     * @return
     */
    private TermsAggregationBuilder createTermsAggregationBuilder(Map<String, String> pailMap, List<BasedCalculationCondition> calculations) {
        TermsAggregationBuilder childTermAgg = null;
        for (int i = pailMap.size(); i > 0; i--) {
            String key = bucketPrefix + i;
            TermsAggregationBuilder termAgg = AggregationBuilders.terms(key).field(pailMap.get(key));
            if (childTermAgg != null) {
                termAgg.subAggregation(childTermAgg);
            } else {
                termAgg = createValueAggregationBuilder(termAgg, calculations);
            }
            childTermAgg = termAgg.size(1000);
        }
        return childTermAgg;
    }


    /**
     * ?????????????????????????????????????????????
     * @param calculations
     * @return
     */
    private TermsAggregationBuilder createValueAggregationBuilder(TermsAggregationBuilder termsAgg, List<BasedCalculationCondition> calculations) {
        for (BasedCalculationCondition condition : calculations) {
            AggregationBuilder valueAgg = null;
            if (ConditionType.SUM.getType().equals( condition.getConditionType().getType())){
                valueAgg = AggregationBuilders.sum(condition.getColumn()).field(condition.getField());
            }
            if (ConditionType.AVG.getType().equals(condition.getConditionType().getType())){
                valueAgg = AggregationBuilders.avg(condition.getColumn()).field(condition.getField());
            }
            if (ConditionType.MAX.getType().equals(condition.getConditionType().getType())){
                valueAgg = AggregationBuilders.max(condition.getColumn()).field(condition.getField());
            }
            if (ConditionType.MIN.getType().equals(condition.getConditionType().getType())){
                valueAgg = AggregationBuilders.min(condition.getColumn()).field(condition.getField());
            }
            if (ConditionType.COUNT.getType().equals(condition.getConditionType().getType())){
                valueAgg = AggregationBuilders.count(condition.getColumn()).field(condition.getField());
            }
            if (ConditionType.CARDINALITY.getType().equals(condition.getConditionType().getType())){
                valueAgg = AggregationBuilders.cardinality(condition.getColumn()).field(condition.getField());
            }
            if (valueAgg != null) {
                termsAgg.subAggregation(valueAgg);
            }
        }
        return termsAgg;
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
                            ReflectUtils.reflectionToFillInData( instanceOfT, entry.getKey(), entry.getValue());
                        }
                    }
                }
            }
            list.add(instanceOfT);
        }
        return list;
    }

    /**
     * ??????????????????
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
     * ??????????????????????????????
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
            // ????????????????????????
            if (childAgg == null){
                basedQueryContext.put("inventoryInfo", inventoryInfo);
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
     * ????????????????????????????????????
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

            //??????????????????
            for (Map.Entry<String, String> entry : inventoryInfo.entrySet()) {
                ReflectUtils.reflectionToFillInData(instanceOfT, entry.getKey(), entry.getValue());
            }
            //????????????????????????
            for (Map.Entry<String, String> entry : sumFields.entrySet()) {
                JSONObject jsonObject = JSON.parseObject(JSON.toJSONString(asMap.get(entry.getKey())));
                Object value = jsonObject.get("value");
                ReflectUtils.reflectionToFillInData(instanceOfT, entry.getKey(), value);
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
