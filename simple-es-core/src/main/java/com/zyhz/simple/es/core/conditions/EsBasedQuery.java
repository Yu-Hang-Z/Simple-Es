package com.zyhz.simple.es.core.conditions;




import com.zyhz.simple.es.common.enums.ConditionType;
import com.zyhz.simple.es.common.model.BasedCalculationCondition;
import com.zyhz.simple.es.common.model.BasedQueryCondition;
import com.zyhz.simple.es.core.base.BasedQueryES;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

/**
 * @author by zhangyuhang
 * @Classname EsBasedQueryRequest
 * @Description ES 基础查询条件
 * @Date 2023/1/9 11:24
 */
@Getter
@Setter
public class EsBasedQuery<T> {

    /**
     * 索引
     */
    private String index;

    /**
     * 查询数量
     */
    private Integer size = 50;

    /**
     * 查询页数
     */
    private Integer from = 0;

    /**
     * 查询条件
     */
    private List<BasedQueryCondition> basedQueryConditions;

    /**
     * 制定返回字段
     */
    private List<String> fetchSource;


    LinkedHashMap<String,String> aggregationMap;

    /**
     * 聚合字段
     * key：Level层数
     * value：返回Java对象对应字段
     */
    private Map<String, String> level3Polymerization;

    /**
     * 聚合字段
     * key：Level层数
     * value：es对应字段
     */
    private Map<String, String> bucketFields;

    /**
     * 聚合求和的污染物
     * key：查询结果接受对象对应污染物字段
     * value：es对应字段污染物
     */
    private Map<String, String> sumFields;

    private List<BasedCalculationCondition> aggregateCalculationConditions;

    /**
     * 必填参数，确定泛型类型
     */
    private Class<T> clz;

    /**
     * 构建聚合条件
     * 顺序需要和聚合顺序一致
     */
    public void buildPolymerizationConditions(LinkedHashMap<String, String> map){
        if (map == null){
            return;
        }
        Map<String, String> level3Polymerization = new HashMap<>();
        Map<String, String> bucketFields = new HashMap<>();
        int i = 1;
        for (Map.Entry<String, String> entry : map.entrySet()){
            String key = BasedQueryES.bucketPrefix + i;
            level3Polymerization.put(key, entry.getKey());
            bucketFields.put(key, entry.getValue());
            i++;
        }
        this.level3Polymerization = level3Polymerization;
        this.bucketFields = bucketFields;
    }

    public void addAggregateCalculationConditions(String conditionType, String field, String value){
        if (this.aggregateCalculationConditions == null){
            this.aggregateCalculationConditions = new ArrayList<>();
        }
        if (value == null){
            return;
        }
        this.aggregateCalculationConditions.add(new BasedCalculationCondition(ConditionType.getConditionType(conditionType),field,value));
    }

    public void addFetchSource(String field){
        if (this.fetchSource == null){
            this.fetchSource = new ArrayList<>();
        }
        this.fetchSource.add(field);
    }

    public void addSumFields(String columns, String field){
        if (this.sumFields == null){
            this.sumFields = new HashMap<>();
        }
        this.sumFields.put(columns, field);
    }

    public void addQueryCondition(String conditionType, String field, Object value){
        if (this.basedQueryConditions == null){
            this.basedQueryConditions = new ArrayList<>();
        }
        if (value == null){
            return;
        }
        this.basedQueryConditions.add(new BasedQueryCondition(ConditionType.getConditionType(conditionType),field,value));
    }

    public void addAggregationMap(String columns, String field){
        if (this.aggregationMap == null){
            this.aggregationMap = new LinkedHashMap<>();
        }
        this.aggregationMap.put(columns, field);
    }

    public void addFetchSource(List<String> fields){
        if (this.fetchSource == null){
            this.fetchSource = new ArrayList<>();
        }
        this.fetchSource.addAll(fields);
    }


}
