package com.zyhz.simple.es.common.model;


import com.zyhz.simple.es.common.enums.ConditionType;
import lombok.Builder;
import lombok.Data;

/**
 * @author by zhangyuhang
 * @Classname BasedQueryConditions
 * @Description TODO
 * @Date 2023/1/13 10:42
 */
@Data
@Builder
public class BasedCalculationCondition {


    private ConditionType conditionType;

    private String column;

    private String field;

    public BasedCalculationCondition(ConditionType conditionType, String column, String field){
        this.conditionType = conditionType;
        this.column = column;
        this.field = field;
    }
}
