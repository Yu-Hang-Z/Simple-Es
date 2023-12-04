package com.zyhz.simple.es.sample.entity;


import com.zyhz.simple.es.annotation.IndexField;
import com.zyhz.simple.es.annotation.IndexName;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author by zhangyuhang
 * @Classname EsMeicInventoryBaseQueryData
 * @Description TODO
 * @Date 2023/11/13 14:40
 */
@Getter
@Setter
@IndexName("idx_emis_server_aggregation_inv_dataes_v1")
public class EsMeicInventoryBaseQueryData implements Serializable {
    /**
     * 清单组
     */
    private static final long serialVersionUID = 1L;

    @IndexField("inv_gid")
    private String inventoryGroupId;

    /**
     * 清单id
     */
    @IndexField("inventory_id")
    private String inventoryId;
    /**
     * 所选年
     */
    @IndexField("year")
    private String year;

    /**
     * 所选月
     */
    @IndexField("month")
    private String month;

    /**
     * 日期
     */
    private String date;

    /**
     * 时间戳
     */
    private String timestamp;


    /**
     * 国家编码
     */
    @IndexField("nation_code")
    private String nationCode;
    /**
     * 区域
     */
    @IndexField("state_code")
    private String stateCode;

    /**
     * 部门
     */
    @IndexField("dim_name")
    private String dimName;

    /**
     * 燃料
     */
    @IndexField("dim_value")
    private String dimValue;

    /**
     * co2
     */
    @IndexField("co2")
    private BigDecimal co2;

    /**
     * bc
     */
    @IndexField("bc")
    private BigDecimal bc;

    /**
     * nh3
     */
    @IndexField("nh3")
    private BigDecimal nh3;

    /**
     * nox
     */
    @IndexField("nox")
    private BigDecimal nox;

    /**
     * oc
     */
    @IndexField("oc")
    private BigDecimal oc;

    /**
     * pm10
     */
    @IndexField("pm10")
    private BigDecimal pm10;

    /**
     * pm25
     */
    @IndexField("pm25")
    private BigDecimal pm25;

    /**
     * so2
     */
    @IndexField("so2")
    private BigDecimal so2;

    /**
     * voc
     */
    @IndexField("voc")
    private BigDecimal voc;

    /**
     * co
     */
    @IndexField("co")
    private BigDecimal co;

    public BigDecimal getAttribute(String attribute) {
        switch (attribute.toLowerCase()) {
            case "co2":
                return this.getCo2();
            // Add more cases for other attributes
            case "bc":
                return this.getBc();
            case "nh3":
                return this.getNh3();
            case "nox":
                return this.getNox();
            case "oc":
                return this.getOc();
            case "pm10":
                return this.getPm10();
            case "pm25":
                return this.getPm25();
            case "so2":
                return this.getSo2();
            case "voc":
                return this.getVoc();
            case "co":
                return this.getCo();
            default:
                throw new IllegalArgumentException("Invalid attribute: " + attribute);
        }
    }
}
