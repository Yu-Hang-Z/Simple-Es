package com.zyhz.simple.es.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

/**
 * @author by zhangyuhang
 * @Classname ConditionType
 * @Description TODO
 * @Date 2023/3/3 13:18
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum ConditionType {

    EQUALS("equals"),

    NOT_EQUALS("not_equals"),

    GT("gt"),

    GTE("gte"),

    LT("gt"),

    LTE("gte"),

    IN("in"),

    NOT_IN("not_in"),

    LIKE("like"),

    NOT_LIKE("not_like"),

    IS_NULL("is_null"),

    IS_NOT_NULL("is_not_null"),

    BETWEEN("between"),

    NOT_BETWEEN("not_between"),

    SUM("sum"),

    MAX("max"),

    MIN("min"),

    COUNT("count"),

    CARDINALITY("cardinality"),

    AVG("avg");


    private String type;


    public static ConditionType getConditionType(String code) {
        for (ConditionType conditionType : ConditionType.values()) {
            if (StringUtils.equals(code, conditionType.getType())) {
                return conditionType;
            }
        }
        return null;
    }
}
