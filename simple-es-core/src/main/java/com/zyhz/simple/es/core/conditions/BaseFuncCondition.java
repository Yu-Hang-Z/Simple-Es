package com.zyhz.simple.es.core.conditions;


import com.zyhz.simple.es.core.conditions.interfaces.BaseCondition;
import lombok.*;
import org.apache.commons.lang3.StringUtils;

/**
 * @author by zhangyuhang
 * @Date 2023/11/21 16:58
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BaseFuncCondition implements BaseCondition {
    @AllArgsConstructor
    @Getter
    public enum ConditionType {
        GROUP_BY("group_by"),

        SOURCE("source"),

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
    private ConditionType conditionType;

    private String field;

    private String column;

}
