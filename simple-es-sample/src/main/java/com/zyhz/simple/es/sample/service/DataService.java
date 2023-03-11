package com.zyhz.simple.es.sample.service;

import com.zyhz.simple.es.core.base.BasedQueryES;
import com.zyhz.simple.es.core.conditions.QueryWrapper;
import com.zyhz.simple.es.sample.entity.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author by zhangyuhang
 * @Classname DataService
 * @Description TODO
 * @Date 2023/3/3 11:34
 */
@Service
public class DataService {

    @Resource
    BasedQueryES basedQueryES;

    public List<Data> getData1() throws IOException {
        QueryWrapper warper = QueryWrapper.create()
                .setQueryIndex(true, "idx_emis_server_aggregation_pop_year_7754_v1")
                .addAllSource("state_code,co2")
                .setQuerySize(true, 5)
                .setQueryFrom(true, 0)
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(warper);
        return list;

    }

    public List<Data> getData2() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex(true, "idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy(true, "state_code", "state_code")
                .sum(true, "co2", "co_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }

    public List<Data> getData3() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex(true, "idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy(true, "state_code", "state_code")
                .groupBy(true, "year", "year")
                .notIn(true, "year", "1998")
                .sum(true, "co2", "co_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }

    public List<Data> getData4() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex(true, "idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy(true, "year", "year")
                .groupBy(true, "state_code", "state_code")
                .sum(true, "co2", "co2")
                .sum(true, "co2_catipa", "co2_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }

    public List<Data> getData5() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex(true, "alias_emis_server_aggregation_pop_year")
                .groupBy(true, "year", "year")
                .groupBy(true, "state_code", "state_code")
                .sum(true, "sum_co2", "co2")
                .max(true, "max_co2", "co2")
                .min(true, "min_co2", "co2")
                .count(true, "count_co2", "co2")
                .avg(true, "avg_co2", "co2")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;

    }

    public List<Data> getData6() throws IOException {
        List<Data> list =  basedQueryES.query(QueryWrapper.create()
                .setQueryIndex(true, "alias_emis_server_aggregation_pop_year")
                .groupBy(true, "year", "year")
                .groupBy(true, "state_code", "state_code")
                .sum(true, "sum_co2", "co2")
                .max(true, "max_co2", "co2")
                .min(true, "min_co2", "co2")
                .count(true, "count_co2", "co2")
                .avg(true, "avg_co2", "co2")
                .addGeneric(Data.class));
        return list;

    }

    public List<Data> getData7() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex(true, "idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy(true, "year", "year")
                .groupBy(true, "state_code", "state_code")
                .sum(true, "co2", "co2")
                .sum(true, "co2_catipa", "co2_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }
}
