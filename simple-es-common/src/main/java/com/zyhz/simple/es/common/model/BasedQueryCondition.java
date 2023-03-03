package com.zyhz.simple.es.common.model;


import com.zyhz.simple.es.common.enums.ConditionType;
import lombok.*;

/**
 * @author by zhangyuhang
 * @Classname BasedQueryConditions
 * @Description TODO
 * @Date 2023/1/13 10:42
 */
@Data
@Builder
public class BasedQueryCondition {


    private ConditionType conditionType;

    private String field;

    private Object value;

    public BasedQueryCondition(ConditionType conditionType, String field, Object value){
        this.conditionType = conditionType;
        this.field = field;
        this.value = value;
    }
}
