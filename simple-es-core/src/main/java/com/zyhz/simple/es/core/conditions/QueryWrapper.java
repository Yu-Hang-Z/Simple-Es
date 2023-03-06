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
 * @Classname QueryWrapper
 * @Description TODO
 * @Date 2023/2/3 10:47
 */
public class QueryWrapper<T> extends EsBasedQuery
        implements Func<QueryWrapper<T>> {



    public static QueryWrapper create(){
        return new QueryWrapper();
    }

    @Override
    public QueryWrapper<T> not(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.NOT_EQUALS.getType(), column, field);
        }
        return this;
    }

    @Override
    public QueryWrapper<T> equals(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.EQUALS.getType(), column, field);
        }
        return this;
    }

    @Override
    public QueryWrapper in(boolean condition, String column, Object val) {
        if(condition && !ObjectUtils.isEmpty(val)){
            this.addQueryCondition(ConditionType.IN.getType(), column, val);
        }
        return this;
    }

    @Override
    public QueryWrapper between(boolean condition, String field, Object from, Object to) {
        if(condition && !ObjectUtils.isEmpty(from) && !ObjectUtils.isEmpty(to)){
            Map<String, Object> map = new HashMap<>();
            map.put("from", from);
            map.put("to", to);
            this.addQueryCondition(ConditionType.FROM_TO.getType(), field, map);
        }
        return this;
    }
    @Override
    public QueryWrapper<T> gt(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.GT.getType(), column, field);
        }
        return this;
    }

    @Override
    public QueryWrapper<T> gte(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.GTE.getType(), column, field);
        }
        return this;
    }

    @Override
    public QueryWrapper<T> lt(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.LT.getType(), column, field);
        }
        return this;
    }

    @Override
    public QueryWrapper<T> lte(boolean condition, String column, Object field) {
        if (condition){
            this.addQueryCondition(ConditionType.LTE.getType(), column, field);
        }
        return this;
    }

    public QueryWrapper groupBy(boolean condition, String columns, String field){
        if (condition && columns != null && field != null){
            this.addAggregationMap(columns,field);
        }
        this.buildPolymerizationConditions(this.aggregationMap);
        return this;
    }

    @Override
    public QueryWrapper<T> sum(boolean condition, String columns, String field) {
        if (condition){
            this.addSumFields(columns,field);
        }
        return this;
    }

    public QueryWrapper from_to(boolean condition, Integer page, Integer size){
        if (condition){
            this.setFrom(page);
            this.setSize(size);
        }
        return this;
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

    public QueryWrapper setQuerySize(boolean condition,Integer size){
        if (condition && size != null){
            this.setSize(size);
        }
        return this;
    }

    public QueryWrapper setQueryFrom(boolean condition,Integer from){
        if (condition && from != null){
            this.setFrom(from);
        }
        return this;
    }


}
