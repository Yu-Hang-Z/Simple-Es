package com.zyhz.simple.es.core.mapper;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;

import com.zyhz.simple.es.annotation.utils.ReflectUtils;
import com.zyhz.simple.es.core.helper.IndexEntityHelper;
import com.zyhz.simple.es.core.mapper.interfaces.BaseEsMapper;
import com.zyhz.simple.es.core.wrapper.LambdaEsQueryWrapper;
import com.zyhz.simple.es.core.wrapper.interfaces.Wrapper;
import lombok.Setter;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.aggregations.Aggregation;
import org.elasticsearch.search.aggregations.Aggregations;
import org.elasticsearch.search.aggregations.bucket.terms.Terms;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;


/**
 * @author by zhangyuhang
 * @Date 2023/11/21 16:16
 */
@Service
public class BaseEsMapperImpl<T> implements BaseEsMapper<T> {

    @Resource
    private RestHighLevelClient restHighLevelClient;

    @Setter
    private Class<T> entityClass;

    public static final String bucketPrefix = "level";


    @Override
    public Class<T> getEntityClass() {
        return entityClass;
    }

    @Override
    public List<T> selectList(Wrapper<T> wrapper) throws IOException {
        //解析 wrapper
        LambdaEsQueryWrapper<T> lambdaEsQueryWrapper = (LambdaEsQueryWrapper<T>) wrapper;
        //entityClass = lambdaEsQueryWrapper.getEntityClass();
        // 0.初始化数据,1.获取 indexName
        Map<String, String> fetchSource = IndexEntityHelper.indexInfoMap.get(lambdaEsQueryWrapper.getEntityClass().getName());
        Map<String, String> reversedMap = fetchSource.entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));

        String index = IndexEntityHelper.indexNameMap.get(lambdaEsQueryWrapper.getEntityClass().getName());
        BasedEsMapperCache basedEsMapperCache = new BasedEsMapperCache(index,lambdaEsQueryWrapper.getPage(), lambdaEsQueryWrapper.getPageSize(), lambdaEsQueryWrapper.getEntityClass(), reversedMap);

        // 2.数据query
        SearchRequest searchRequest = basedEsMapperCache.buildData(lambdaEsQueryWrapper.getBasedQueryConditions(), index, lambdaEsQueryWrapper.getEntityClass());

        SearchResponse searchResponse = restHighLevelClient.search(searchRequest, RequestOptions.DEFAULT);
        basedEsMapperCache.initContext();
        List<T> esBasedQueryDataList = getSC(searchResponse, basedEsMapperCache.getBasedQueryContext());

        return esBasedQueryDataList;
    }

    @Override
    public List<T> selectAggregateList(Wrapper<T> wrapper) throws IOException {
        //解析 wrapper
        LambdaEsQueryWrapper<T> lambdaEsQueryWrapper = (LambdaEsQueryWrapper<T>) wrapper;
        //entityClass = lambdaEsQueryWrapper.getEntityClass();
        // 0.初始化数据,1.获取 indexName
        String index = IndexEntityHelper.indexNameMap.get(lambdaEsQueryWrapper.getEntityClass().getName());
        BasedEsMapperCache basedEsMapperCache = new BasedEsMapperCache(index, lambdaEsQueryWrapper.getPage(), lambdaEsQueryWrapper.getPageSize(), lambdaEsQueryWrapper.getEntityClass());

        // 2.数据query
        SearchRequest searchRequest = basedEsMapperCache.buildData(lambdaEsQueryWrapper.getBasedQueryConditions(), index, lambdaEsQueryWrapper.getEntityClass());

        SearchResponse searchResponse = restHighLevelClient.search(searchRequest, RequestOptions.DEFAULT);
        basedEsMapperCache.initContext();
        List<T> esBasedQueryDataList = analysisBuckets(searchResponse, basedEsMapperCache.getBasedQueryContext());

        return esBasedQueryDataList;
    }


    /**
     * 解析聚合信息
     *
     * @param searchResponse
     * @return
     */
    private List<T> analysisBuckets(SearchResponse searchResponse, Map<String, Object> basedQueryContext) {
        List<T> esBasedQueryDataList = new ArrayList<>();
        String key = bucketPrefix + 1;
        if (searchResponse.getAggregations().get(key) != null) {
            Terms aggMap = searchResponse.getAggregations().get(key);
            List<T> inventoryAggDtoList = analysisLevelBuckets(aggMap,1, basedQueryContext);
            esBasedQueryDataList.addAll(inventoryAggDtoList);
        }
        return esBasedQueryDataList;
    }


    /**
     * 递归动态解析聚合信息
     */
    private List<T> analysisLevelBuckets(Terms terms, int recursionNumber, Map<String, Object> basedQueryContext) {
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
                basedQueryContext.put("inventoryInfo", inventoryInfo);
                List<T> esBasedQueryData = analysisLastBuckets(terms, recursionNumber, basedQueryContext);
                analysisLevelList.addAll(esBasedQueryData);
                return esBasedQueryData;
            }
            String date = bucket.getKeyAsString();
            String key = bucketPrefix + recursionNumber;
            inventoryInfo.put(level3Polymerization.get(key), date);
            basedQueryContext.put("inventoryInfo", inventoryInfo);
            List<T> analysisLevel3List = analysisLevelBuckets(childAgg, recursionNumber + 1, basedQueryContext);
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
    private List<T> analysisLastBuckets(Terms terms, int recursionNumber, Map<String, Object> basedQueryContext) {
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
                ReflectUtils.reflectionToFillInData(instanceOfT, entry.getKey(), entry.getValue());
            }
            //聚合计算字段字段
            for (Map.Entry<String, String> entry : sumFields.entrySet()) {
                JSONObject jsonObject = JSON.parseObject(JSON.toJSONString(asMap.get(entry.getKey())));
                Object value = jsonObject.get("value");
                ReflectUtils.reflectionToFillInData(instanceOfT, entry.getKey(), value);
            }
            analysisLastList.add(instanceOfT);
        }
        return analysisLastList;
    }

    private List<T> getSC(SearchResponse sr, Map<String, Object> basedQueryContext) {
        List<T> list = new ArrayList<T>();
        Class<T> clz = (Class<T>) basedQueryContext.get("clz");
        Map<String,String> fetchSource = ( Map<String,String>) basedQueryContext.get("fetchSource");
        for (SearchHit hit : sr.getHits()) {
            Map<String, Object> source = hit.getSourceAsMap();
            T instanceOfT = this.getInstanceOfT(clz);
            if (!source.isEmpty()) {
                for (Iterator<Map.Entry<String, Object>> it = source.entrySet().iterator(); it.hasNext(); ) {
                    Map.Entry<String, Object> entry = it.next();
                    if ( fetchSource.containsKey(entry.getKey()) ){
                        String fieldType = ReflectUtils.getFieldType(instanceOfT, fetchSource.get(entry.getKey()));
                        reflectionToFillInData(fieldType, instanceOfT, fetchSource.get(entry.getKey()), entry.getValue());
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
            if (value == null){
                ReflectUtils.setFieldValue(instanceOfT, fieldName, null);
                return;
            }
            ReflectUtils.setFieldValue(instanceOfT, fieldName, BigDecimal.valueOf((Double) value));
        }

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
