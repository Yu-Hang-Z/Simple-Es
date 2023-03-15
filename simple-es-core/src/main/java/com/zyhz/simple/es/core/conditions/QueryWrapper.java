package com.zyhz.simple.es.core.conditions;



import com.zyhz.simple.es.common.enums.ConditionType;
import com.zyhz.simple.es.core.conditions.interfaces.Func;
import org.apache.commons.lang3.ObjectUtils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author by zhangyuhang

 */
public class QueryWrapper<T> extends EsBasedQuery
        implements Func<QueryWrapper<T>> {



    public static QueryWrapper create(){
        return new QueryWrapper();
    }

    public QueryWrapper<T> index(String index){
        this.setIndex(index);
        return this;
    }
    @Override
    public QueryWrapper<T> ne(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.NOT_EQUALS.getType(), column, field);
        }
        return this;
    }
    public QueryWrapper<T> ne(String column, Object field) {
        return ne(true, column, field);
    }

    @Override
    public QueryWrapper<T> eq(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.EQUALS.getType(), column, field);
        }
        return this;
    }
    public QueryWrapper<T> eq(String column, Object field) {
        return eq(true, column, field);
    }

    @Override
    public QueryWrapper in(boolean condition, String column, Object val) {
        if(condition && !ObjectUtils.isEmpty(val)){
            this.addQueryCondition(ConditionType.IN.getType(), column, val);
        }
        return this;
    }
    public QueryWrapper in(String column, Object val) {
        return in(true, column, val);
    }

    @Override
    public QueryWrapper notIn(boolean condition, String column, Object val) {
        if(condition && !ObjectUtils.isEmpty(val)){
            this.addQueryCondition(ConditionType.NOT_IN.getType(), column, val);
        }
        return this;
    }
    public QueryWrapper notIn(String column, Object val) {
        return notIn(true, column, val);
    }

    @Override
    public QueryWrapper<T> like(boolean condition, String column, String val) {
        if (condition && !ObjectUtils.isEmpty(val)){
            this.addQueryCondition(ConditionType.LIKE.getType(), column, val);
        }
        return this;
    }
    public QueryWrapper<T> like(String column, String val) {
        return like(true, column, val);
    }

    @Override
    public QueryWrapper<T> notLike(boolean condition, String column, String val) {
        if (condition && !ObjectUtils.isEmpty(val)){
            this.addQueryCondition(ConditionType.NOT_LIKE.getType(), column, val);
        }
        return this;
    }
    public QueryWrapper<T> notLike(String column, String val) {
        return notLike(true, column, val);
    }

    @Override
    public QueryWrapper<T> isNull(boolean condition, String column, Object val) {
        if (condition && !ObjectUtils.isEmpty(val)){
            this.addQueryCondition(ConditionType.IS_NULL.getType(), column, val);
        }
        return this;
    }
    public QueryWrapper<T> isNull(String column, Object val) {
        return isNull(true, column, val);
    }

    @Override
    public QueryWrapper<T> isNotNull(boolean condition, String column, Object val) {
        if (condition && !ObjectUtils.isEmpty(val)){
            this.addQueryCondition(ConditionType.IS_NOT_NULL.getType(), column, val);
        }
        return this;
    }
    public QueryWrapper<T> isNotNull(String column, Object val) {
        return isNotNull(true, column, val);
    }


    @Override
    public QueryWrapper between(boolean condition, String field, Object from, Object to) {
        if(condition && !ObjectUtils.isEmpty(from) && !ObjectUtils.isEmpty(to)){
            Map<String, Object> map = new HashMap<>();
            map.put("from", from);
            map.put("to", to);
            this.addQueryCondition(ConditionType.BETWEEN.getType(), field, map);
        }
        return this;
    }
    public QueryWrapper between(String field, Object from, Object to) {
        return between(true, field, from, to);
    }

    @Override
    public QueryWrapper<T> notBetween(boolean condition, String field, Object from, Object to) {
        if (condition && !ObjectUtils.isEmpty(from) && !ObjectUtils.isEmpty(to)){
            Map<String, Object> map = new HashMap<>();
            map.put("from", from);
            map.put("to", to);
            this.addQueryCondition(ConditionType.NOT_BETWEEN.getType(), field, map);
        }
        return this;
    }
    public QueryWrapper<T> notBetween(String field, Object from, Object to) {
        return notBetween(true, field, from, to);
    }

    @Override
    public QueryWrapper<T> gt(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.GT.getType(), column, field);
        }
        return this;
    }
    public QueryWrapper<T> gt(String column, Object field) {
        return gt(true, column, field);
    }

    @Override
    public QueryWrapper<T> ge(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.GTE.getType(), column, field);
        }
        return this;
    }
    public QueryWrapper<T> ge(String column, Object field) {
        return ge(true, column, field);
    }

    @Override
    public QueryWrapper<T> lt(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.LT.getType(), column, field);
        }
        return this;
    }
    public QueryWrapper<T> lt(String column, Object field) {
        return lt(true, column, field);
    }

    @Override
    public QueryWrapper<T> le(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.LTE.getType(), column, field);
        }
        return this;
    }
    public QueryWrapper<T> le(String column, Object field) {
        return le(true, column, field);
    }

    public QueryWrapper groupBy(boolean condition, String columns, String field){
        if (condition && columns != null && field != null){
            this.addAggregationMap(columns,field);
        }
        this.buildPolymerizationConditions(this.aggregationMap);
        return this;
    }
    public QueryWrapper groupBy(String columns, String field){
        return groupBy(true, columns, field);
    }

    @Override
    public QueryWrapper<T> sum(boolean condition, String column, String field) {
        if (condition){
            this.addAggregateCalculationConditions(ConditionType.SUM.getType(), column, field);
            this.addSumFields(column,field);
        }
        return this;
    }
    public QueryWrapper<T> sum(String column, String field) {
        return sum(true, column, field);
    }

    @Override
    public QueryWrapper<T> min(boolean condition, String column, String field) {
        if (condition){
            this.addAggregateCalculationConditions(ConditionType.MIN.getType(), column, field);
            this.addSumFields(column,field);
        }
        return this;
    }
    public QueryWrapper<T> min(String column, String field) {
        return min(true, column, field);
    }

    @Override
    public QueryWrapper<T> max(boolean condition, String column, String field) {
        if (condition){
            this.addAggregateCalculationConditions(ConditionType.MAX.getType(), column, field);
            this.addSumFields(column,field);
        }
        return this;
    }
    public QueryWrapper<T> max(String column, String field) {
        return max(true, column, field);
    }

    @Override
    public QueryWrapper<T> count(boolean condition, String column, String field) {
        if (condition){
            this.addAggregateCalculationConditions(ConditionType.COUNT.getType(), column, field);
            this.addSumFields(column,field);
        }
        return this;
    }
    public QueryWrapper<T> count(String column, String field) {
        return count(true, column, field);
    }

    @Override
    public QueryWrapper<T> avg(boolean condition, String column, String field) {
        if (condition){
            this.addAggregateCalculationConditions(ConditionType.AVG.getType(), column, field);
            this.addSumFields(column,field);
        }
        return this;
    }

    public QueryWrapper<T> avg(String column, String field) {
        return avg(true, column, field);
    }

    public QueryWrapper from_to(boolean condition, Integer page, Integer size){
        if (condition){
            this.setFrom(page);
            this.setSize(size);
        }
        return this;
    }
    public QueryWrapper from_to(Integer page, Integer size){
        return from_to(true, page, size);
    }

    public QueryWrapper sort(boolean condition, String field, String type){
        return this;
    }

    public QueryWrapper addSource(String field){
        if (field != null && !"".equals(field)){
            this.addFetchSource(field);
        }
        return this;
    }

    public QueryWrapper addAllSource(String fields){
        if (fields != null && !"".equals(fields)){
            List<String> fieldList = Arrays.stream(fields.split(",")).collect(Collectors.toList());
            this.addFetchSource(fieldList);
        }
        return this;
    }

    public QueryWrapper addGeneric(Class<T> clz){
        if (clz != null){
            this.setClz(clz);
        }
        return this;
    }

    public QueryWrapper setQueryIndex(boolean condition,String index){
        if (condition && index != null){
            this.setIndex(index);
        }
        return this;
    }
    public QueryWrapper setQueryIndex(String type){
        return setQueryIndex(true,type);
    }

    public QueryWrapper setQuerySize(boolean condition,Integer size){
        if (condition && size != null){
            this.setSize(size);
        }
        return this;
    }
    public QueryWrapper setQuerySize(Integer size){
        return setQuerySize(true,size);
    }

    public QueryWrapper setQueryFrom(boolean condition,Integer from){
        if (condition && from != null){
            this.setFrom(from);
        }
        return this;
    }
    public QueryWrapper setQueryFrom(Integer from){
        return setQueryFrom(true,from);
    }


}
