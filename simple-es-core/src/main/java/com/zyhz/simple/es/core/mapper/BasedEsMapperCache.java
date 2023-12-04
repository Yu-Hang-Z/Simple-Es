package com.zyhz.simple.es.core.mapper;


import com.zyhz.simple.es.annotation.Const.EsConst;
import com.zyhz.simple.es.core.conditions.BaseFuncCondition;
import com.zyhz.simple.es.core.conditions.BasedQueryCondition;
import com.zyhz.simple.es.core.conditions.interfaces.BaseCondition;
import com.zyhz.simple.es.core.helper.IndexEntityHelper;
import lombok.Getter;
import lombok.Setter;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.aggregations.AggregationBuilder;
import org.elasticsearch.search.aggregations.AggregationBuilders;
import org.elasticsearch.search.aggregations.bucket.terms.TermsAggregationBuilder;

import org.elasticsearch.search.aggregations.metrics.SumAggregationBuilder;
import org.elasticsearch.search.builder.SearchSourceBuilder;

import java.io.IOException;
import java.util.*;

/**
 * @author by zhangyuhang
 * @Date 2023/11/28 11:00
 */
public class BasedEsMapperCache<T> {

    public static final String bucketPrefix = "level";

    @Setter
    private Class<T> entityClass;

    /**
     * 索引
     */
    private String index;

    /**
     * 查询数量
     */
    private Integer size = 10;

    private Integer page = 1;

    /**
     * 制定返回字段
     */
    private List<String> fetchSource;

    private Map<String,String> source;

    LinkedHashMap<String,String> aggregationMap;

    /**
     * 聚合字段
     * key：Level层数
     * value：返回Java对象对应字段
     */
    private Map<String, String> levelAndColumnMap;

    /**
     * 聚合字段
     * key：Level层数
     * value：es对应字段
     */
    private Map<String, String> levelAndFieldMap;

    /**
     * 聚合求和的污染物
     * key：查询结果接受对象对应污染物字段
     * value：es对应字段污染物
     */
    private Map<String, String> sumFields;


    @Getter
    private Map<String, Object> basedQueryContext = new HashMap<>();

    public BasedEsMapperCache(String index, Integer page, Integer pageSize, Class<T> clz){
        this.index = index;
        this.size = pageSize;
        this.page = page;
        this.fetchSource = new ArrayList<>();
        this.source = new HashMap<>();
        this.aggregationMap = new LinkedHashMap<>();
        this.levelAndColumnMap = new LinkedHashMap<>();
        this.levelAndFieldMap = new LinkedHashMap<>();
        this.sumFields = new LinkedHashMap<>();
        this.entityClass = clz;
    }

    public BasedEsMapperCache(String index, Integer page, Integer pageSize, Class<T> clz, Map<String, String> source){
        this.index = index;
        this.size = pageSize;
        this.page = page;
        this.fetchSource = new ArrayList<>();
        this.source = source;
        this.aggregationMap = new LinkedHashMap<>();
        this.levelAndColumnMap = new LinkedHashMap<>();
        this.levelAndFieldMap = new LinkedHashMap<>();
        this.sumFields = new LinkedHashMap<>();
        this.entityClass = clz;
    }


    public SearchRequest buildData(List<BaseCondition> baseConditions, String index, Class<T> clz) throws IOException {

        // baseConditions 按照BaseFuncCondition 和 BasedQueryCondition 分类
        List<BaseFuncCondition> funcConditions = new ArrayList<>();
        List<BasedQueryCondition> queryConditions = new ArrayList<>();
        for (BaseCondition condition : baseConditions){
            if (condition instanceof BaseFuncCondition){
                funcConditions.add((BaseFuncCondition) condition);
            }
            if (condition instanceof BasedQueryCondition){
                queryConditions.add((BasedQueryCondition) condition);
            }
        }
        SearchRequest searchRequest = new SearchRequest(index);
        //构建查询
        SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();
        // 1.构建查询条件
        if (!queryConditions.isEmpty()){
            BoolQueryBuilder queryBuilder = buildQueryConditions(queryConditions, clz);
            sourceBuilder.query(queryBuilder);

        }
        // 2.构建聚合条件
        if (!funcConditions.isEmpty()){
            sourceBuilder = buildPolymerizationConditions(sourceBuilder, funcConditions, clz);
        }
        // 构建返回source
        String[] source = (String[]) this.fetchSource.toArray(new String[fetchSource.size()]);
        sourceBuilder.fetchSource(source,null);
        sourceBuilder.size(this.size);
        sourceBuilder.from((this.page - 1) * this.size);
        searchRequest.source(sourceBuilder);
        return searchRequest;
    }

    private SearchSourceBuilder buildPolymerizationConditions(SearchSourceBuilder sourceBuilder, List<BaseFuncCondition> funcConditions, Class<T> clz){
        for (BaseFuncCondition condition : funcConditions){
            if (BaseFuncCondition.ConditionType.GROUP_BY.getType().equals(condition.getConditionType().getType())){
                String columns = condition.getColumn();
                String field = condition.getField();
                field = IndexEntityHelper.indexInfoMap.get(clz.getName()).get(columns);
                this.aggregationMap.put(columns,field);
                this.fetchSource.add(field);
                this.source.put(field,columns);
            }
            if (BaseFuncCondition.ConditionType.SUM.getType().equals(condition.getConditionType().getType())){
                String columns = condition.getColumn();
                String field = condition.getField();
                field = IndexEntityHelper.indexInfoMap.get(clz.getName()).get(columns);
                this.sumFields.put(columns,field);
                this.fetchSource.add(field);
                this.source.put(field,columns);
            }
            if (BaseFuncCondition.ConditionType.SOURCE.getType().equals(condition.getConditionType().getType())){
                String columns = condition.getColumn();
                String field = condition.getField();
                field = IndexEntityHelper.indexInfoMap.get(clz.getName()).get(columns);
                this.fetchSource.add(field);
                this.source.put(field,columns);
            }
        }
        buildPolymerizationConditions(this.aggregationMap);
        sourceBuilder = createCitySearchSourceBuilder(sourceBuilder, this.levelAndFieldMap, this.sumFields);
        return sourceBuilder;
    }

    public void buildPolymerizationConditions(LinkedHashMap<String, String> map){
        if (map == null){
            return;
        }
        Map<String, String> level3Polymerization = new HashMap<>();
        Map<String, String> bucketFields = new HashMap<>();
        int i = 1;
        for (Map.Entry<String, String> entry : map.entrySet()){
            String key = EsConst.bucketPrefix + i;
            level3Polymerization.put(key, entry.getKey());
            bucketFields.put(key, entry.getValue());
            i++;
        }
        this.levelAndColumnMap = level3Polymerization;
        this.levelAndFieldMap = bucketFields;
    }
    private SearchSourceBuilder createCitySearchSourceBuilder(SearchSourceBuilder sourceBuilder, Map<String, String> pailMap, Map<String, String> sumMap) {
        if (pailMap == null || pailMap.isEmpty() || sumMap == null || sumMap.isEmpty()) {
            return sourceBuilder;
        }
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
        AggregationBuilder childTermAgg = null;
        List<SumAggregationBuilder> sumAggList = createSumAggregationBuilder(sumMap);
        for (int i = pailMap.size(); i > 0; i--) {
            String key = bucketPrefix + i;
            TermsAggregationBuilder termAgg = AggregationBuilders.terms(key).field(pailMap.get(key));
            if (childTermAgg == null && !sumAggList.isEmpty()){
                for (SumAggregationBuilder sumAgg : sumAggList){
                    termAgg.subAggregation(sumAgg);
                }
            }
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
    private List<SumAggregationBuilder> createSumAggregationBuilder(Map<String, String> sumMap) {
        List<SumAggregationBuilder> sumAggList = new ArrayList<>();

        for (Map.Entry<String, String> entry : sumMap.entrySet()) {
            SumAggregationBuilder sumAgg = AggregationBuilders.sum(entry.getKey()).field(entry.getValue());
            sumAggList.add(sumAgg);
        }

        return sumAggList;
    }



    private BoolQueryBuilder buildQueryConditions(List<BasedQueryCondition> conditions, Class<T> clz){
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        for (BasedQueryCondition condition : conditions){
            String field = condition.getField();
            field = IndexEntityHelper.indexInfoMap.get(clz.getName()).get(field);
            condition.setField(field);
            if (BasedQueryCondition.ConditionType.IN.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                if (values instanceof String) {
                    String value = (String) values;
                    Object[] arr = Arrays.stream(value.split(",")).toArray();
                    queryBuilder.filter(QueryBuilders.termsQuery(condition.getField(), arr));
                } else if (values instanceof List) {
                    List value = (List) values;
                    queryBuilder.filter(QueryBuilders.termsQuery(condition.getField(), value));
                } else {
                    System.out.println("Value is of another type");
                }

            }
            if (BasedQueryCondition.ConditionType.EQUALS.getType().equals( condition.getConditionType().getType())){
                Object values = condition.getValue();
                queryBuilder.filter(QueryBuilders.termQuery(condition.getField(), values));
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

    public void initContext(){
        basedQueryContext = new HashMap<>();
        basedQueryContext.put("bucketFields", this.levelAndFieldMap);
        basedQueryContext.put("sumFields",  this.sumFields);
        basedQueryContext.put("level3Polymerization", this.levelAndColumnMap);
        basedQueryContext.put("fetchSource", this.source);
        basedQueryContext.put("clz", this.entityClass);
    }
}
