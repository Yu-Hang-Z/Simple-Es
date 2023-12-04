package com.zyhz.simple.es.core.wrapper.interfaces;



import com.zyhz.simple.es.core.conditions.BaseFuncCondition;
import com.zyhz.simple.es.core.conditions.BasedQueryCondition;
import com.zyhz.simple.es.core.func.Compare;
import com.zyhz.simple.es.core.conditions.interfaces.BaseCondition;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.util.Assert;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author by zhangyuhang
 * @Date 2023/11/21 15:17
 */
public abstract class AbstractWrapper<T,R,Children extends AbstractWrapper<T, R, Children>> extends Wrapper<T>
        implements Compare<Children,R> {

    protected final Children typedThis = (Children) this;


    public Children setEntity(T entity) {
        this.entity = entity;
        this.initEntityClass();
        return typedThis;
    }

    public Children setEntityClass(Class<T> entityClass) {
        this.entityClass = entityClass;
        this.initEntityClass();
        return typedThis;
    }

    protected void initEntityClass() {
        if (this.entityClass == null && this.entity != null) {
            this.entityClass = (Class<T>) entity.getClass();
        }
    }

    protected Class<T> getCheckEntityClass() {
        Assert.notNull(entityClass, "entityClass must not null,please set entity before use this method!");
        return entityClass;
    }


    protected final void initNeed() {
        basedQueryConditions = new LinkedList<>();
    }

    @Override
    public Children in(boolean condition, String field, Object value) {
        if(condition && !ObjectUtils.isEmpty(value)){
            addBaseQueryParam(BasedQueryCondition.ConditionType.IN.getType(), field, value);
        }
        return typedThis;
    }

    @Override
    public Children between(boolean condition, String field, Object from, Object to) {
        if(condition && !ObjectUtils.isEmpty(from) && !ObjectUtils.isEmpty(to)){
            Map<String, Object> map = new HashMap<>();
            map.put("from", from);
            map.put("to", to);
            addBaseQueryParam(BasedQueryCondition.ConditionType.FROM_TO.getType(), field, map);
        }
        return typedThis;
    }

    @Override
    public Children not(boolean condition, String field, Object value) {
        if(condition && !ObjectUtils.isEmpty(value)){
            addBaseQueryParam(BasedQueryCondition.ConditionType.NOT_EQUALS.getType(), field, value);
        }
        return typedThis;
    }

    @Override
    public Children equals(boolean condition, String field, Object value) {
        if(condition && !ObjectUtils.isEmpty(value)){
            addBaseQueryParam(BasedQueryCondition.ConditionType.EQUALS.getType(), field, value);
        }
        return typedThis;
    }

    @Override
    public Children lessThan(boolean condition, String field, Object value) {
        if(condition && !ObjectUtils.isEmpty(value)){
            addBaseQueryParam(BasedQueryCondition.ConditionType.LT.getType(), field, value);
        }
        return typedThis;
    }

    @Override
    public Children lessThanOrEquals(boolean condition, String field, Object value) {
        if(condition && !ObjectUtils.isEmpty(value)){
            addBaseQueryParam(BasedQueryCondition.ConditionType.LE.getType(), field, value);
        }
        return typedThis;
    }

    @Override
    public Children greaterThan(boolean condition, String field, Object value) {
        if(condition && !ObjectUtils.isEmpty(value)){
            addBaseQueryParam(BasedQueryCondition.ConditionType.GT.getType(), field, value);
        }
        return typedThis;
    }

    @Override
    public Children greaterThanOrEquals(boolean condition, String field, Object value) {
        if(condition && !ObjectUtils.isEmpty(value)){
            addBaseQueryParam(BasedQueryCondition.ConditionType.GE.getType(), field, value);
        }
        return typedThis;
    }

    private void addBaseQueryParam(String conditionType, String field, Object value){
        this.basedQueryConditions.add(BasedQueryCondition.builder()
                .conditionType(BasedQueryCondition.ConditionType.getConditionType(conditionType))
                .field(field)
                .value(value)
                .build());
    }

    @Override
    public Children groupBy(boolean condition, String columns, String field) {
        if (condition && columns != null && field != null){
            String conditionType = BaseFuncCondition.ConditionType.GROUP_BY.getType();
            addBaseFuncParam(conditionType, columns, field);
        }
        return typedThis;
    }

    @Override
    public Children sum(boolean condition, String columns, String field) {
        if (condition && columns != null && field != null){
            addBaseFuncParam(BaseFuncCondition.ConditionType.SUM.getType(), columns, field);
        }
        return typedThis;
    }

    @Override
    public Children sum(boolean condition, List<String> columns) {
        if (condition && columns != null && !columns.isEmpty()){
            for (String column : columns) {
                addBaseFuncParam(BaseFuncCondition.ConditionType.SUM.getType(), column, column);
            }
        }
        return typedThis;
    }


   @Override
    public Children source(boolean condition, List<String> columns) {
        if (condition && columns != null && !columns.isEmpty()){
            for (String column : columns) {
                addBaseFuncParam(BaseFuncCondition.ConditionType.SOURCE.getType(), column, column);
            }
        }
        return typedThis;
    }

    private void addBaseFuncParam(String conditionType, String column, String field){
        this.basedQueryConditions.add(BaseFuncCondition.builder()
                .conditionType(BaseFuncCondition.ConditionType.getConditionType(conditionType))
                .field(field)
                .column(column)
                .build());
    }

    /**
     * 设置分页大小和页码
     * @return
     */
    public Children setPage(Integer page, Integer pageSize) {
        this.page = page;
        this.pageSize = pageSize;
        return typedThis;
    }


    public List<BaseCondition> getBasedQueryConditions() {
        return basedQueryConditions;
    }

    public Class<T> getEntityClass() {
        return entityClass;
    }


}
