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

    NOT_IN("not_in"),

    IN("in"),

    FROM_TO("from_to"),

    GT("gt"),

    GTE("gte"),

    LT("gt"),

    LTE("gte"),

    EQUALS("equals"),

    NOT_EQUALS("not_equals"),


    SUM("sum");



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
