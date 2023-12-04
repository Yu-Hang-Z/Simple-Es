package com.zyhz.simple.es.sample.controller;

import com.zyhz.simple.es.core.mapper.interfaces.BaseEsMapper;
import com.zyhz.simple.es.core.wrapper.EsWrappers;
import com.zyhz.simple.es.core.wrapper.LambdaEsQueryWrapper;
import com.zyhz.simple.es.sample.entity.EsMeicInventoryBaseQueryData;
import com.zyhz.simple.es.sample.entity.MeicInventoryQueryParam;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.List;

/**
 * @author by zhangyuhang
 * @Date 2023/12/4 15:25
 */
@RestController
public class EsInventoryController {
    @Resource
    BaseEsMapper baseEsMapper;

    @RequestMapping("/test")
    public List<EsMeicInventoryBaseQueryData> test() throws IOException {
        MeicInventoryQueryParam param = new MeicInventoryQueryParam();
        param.setAggDim("sector2");
        param.setYears("2019");
        param.setGroupIdList("4801");
        LambdaEsQueryWrapper<EsMeicInventoryBaseQueryData> lambdaEsIndexWrapper = EsWrappers.lambdaQuery(EsMeicInventoryBaseQueryData.class);
        lambdaEsIndexWrapper
                // 过滤条件
                .in(EsMeicInventoryBaseQueryData::getInventoryGroupId, param.getGroupIdList())
                .in(EsMeicInventoryBaseQueryData::getDimName, param.getAggDim())
                //.in(EsMeicInventoryBaseQueryData::getDimValue, param.getElement())
                .in(EsMeicInventoryBaseQueryData::getYear, param.getYears())
                // 聚合
                .groupBy(EsMeicInventoryBaseQueryData::getInventoryGroupId)
                .groupBy(EsMeicInventoryBaseQueryData::getYear)
                .groupBy(EsMeicInventoryBaseQueryData::getNationCode)
                //返回实体
                .sum(EsMeicInventoryBaseQueryData::getCo2)
                .sum(EsMeicInventoryBaseQueryData::getSo2)
                .sum(EsMeicInventoryBaseQueryData::getNox)
                .sum(EsMeicInventoryBaseQueryData::getVoc)
                .sum(EsMeicInventoryBaseQueryData::getNh3)
                .sum(EsMeicInventoryBaseQueryData::getPm25)
                .sum(EsMeicInventoryBaseQueryData::getPm10)
                .sum(EsMeicInventoryBaseQueryData::getBc)
                .sum(EsMeicInventoryBaseQueryData::getOc)
                .sum(EsMeicInventoryBaseQueryData::getCo)
                .sum(EsMeicInventoryBaseQueryData::getSo2);

        List<EsMeicInventoryBaseQueryData> esList = baseEsMapper.selectAggregateList(lambdaEsIndexWrapper);
        System.out.println(esList);
        return esList;
    }

    @RequestMapping("/test2")
    public List<EsMeicInventoryBaseQueryData> test2() throws IOException {
        MeicInventoryQueryParam param = new MeicInventoryQueryParam();
        param.setAggDim("sector2");
        param.setYears("2019");
        param.setGroupIdList("4801");
        LambdaEsQueryWrapper<EsMeicInventoryBaseQueryData> lambdaEsIndexWrapper = EsWrappers.lambdaQuery(EsMeicInventoryBaseQueryData.class);
        lambdaEsIndexWrapper
                .setPage(1, 10)
                // 过滤条件
                .in(EsMeicInventoryBaseQueryData::getInventoryGroupId, param.getGroupIdList())
                .in(EsMeicInventoryBaseQueryData::getDimName, param.getAggDim())
                //.in(EsMeicInventoryBaseQueryData::getDimValue, param.getElement())
                .in(EsMeicInventoryBaseQueryData::getYear, param.getYears());
        List<EsMeicInventoryBaseQueryData> esList = baseEsMapper.selectList(lambdaEsIndexWrapper);
        System.out.println(esList);
        return esList;
    }
}
