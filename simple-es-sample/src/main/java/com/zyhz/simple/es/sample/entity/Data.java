package com.zyhz.simple.es.sample.entity;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author by zhangyuhang
 * @Classname Data
 * @Description TODO
 * @Date 2023/3/3 11:32
 */
@lombok.Data
public class Data implements Serializable {

    private String state_code;

    private BigDecimal co2;

    private Integer year;

    private BigDecimal co2_catipa;

}
