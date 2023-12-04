package com.zyhz.simple.es.sample.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * @author by zhangyuhang
 * @Classname MeicInventoryQueryParam
 * @Description TODO
 * @Date 2023/11/13 13:44
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MeicInventoryQueryParam implements Serializable {


    /**
     * 查询sector、fuel、internation
     */
    private String aggDim;


    /**
     * 查询对应sector、fuel、internation的分类
     */

    private List<String> element;

    /**
     * 查询所需的污染物
     */
    private String items;
    /**
     * 查询年数据还是月数据
     */
    private String timeType;

    /**
     * 查询版本
     */
    private String version;

    /**
     * 查询开始时间
     */
    private String start;

    /**
     * 查询结束时间
     */
    private String end;

    /**
     * 国家编码
     */
    private String nationCode;
    /**
     * 区域编码
     */
    private String stateCode;
    /**
     * 查询年
     */

    private String years;

    /**
     * 根据条件获取清单组id
     */
    private String groupIdList;



}
