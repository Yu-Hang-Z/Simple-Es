package com.zyhz.simple.es.core.conditions;

import com.zyhz.simple.es.core.conditions.interfaces.BaseCondition;
import lombok.*;
import org.apache.commons.lang3.StringUtils;

/**
 * @author by zhangyuhang
 * @Classname BasedQueryConditions
 * @Description 过滤条件构建类
 * @Date 2023/1/13 10:42
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BasedQueryCondition implements BaseCondition {

    @AllArgsConstructor
    @Getter
    public enum ConditionType {
        IN("in"),

        FROM_TO("from_to"),

        EQUALS("equals"),

        LT("less_than"),

        GT("greater_than"),

        LE("less_than_or_equals"),

        GE("less_than_or_equals"),

        NOT_EQUALS("not_equals");



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

    private ConditionType conditionType;

    private String field;

    private Object value;
}
