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
                .setQueryIndex("idx_emis_server_aggregation_pop_year_7754_v1")
                .addAllSource("state_code,co2")
                .setQuerySize(5)
                .setQueryFrom(0)
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(warper);
        return list;

    }

    public List<Data> getData2() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex("idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy("state_code", "state_code")
                .sum("co2", "co_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }

    public List<Data> getData3() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex("idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy("state_code", "state_code")
                .groupBy( "year", "year")
                .notIn( "year", "1998")
                .sum("co2", "co_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }

    public List<Data> getData4() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex("idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy( "year", "year")
                .groupBy( "state_code", "state_code")
                .sum( "co2", "co2")
                .sum( "co2_catipa", "co2_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }

    public List<Data> getData5() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex( "alias_emis_server_aggregation_pop_year")
                .groupBy("year", "year")
                .groupBy( "state_code", "state_code")
                .sum( "sum_co2", "co2")
                .max("max_co2", "co2")
                .min( "min_co2", "co2")
                .count( "count_co2", "co2")
                .avg("avg_co2", "co2")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;

    }

    public List<Data> getData6() throws IOException {
        List<Data> list =  basedQueryES.query(QueryWrapper.create()
                .setQueryIndex("alias_emis_server_aggregation_pop_year")
                .groupBy( "year", "year")
                .groupBy( "state_code", "state_code")
                .sum("sum_co2", "co2")
                .max( "max_co2", "co2")
                .min( "min_co2", "co2")
                .count( "count_co2", "co2")
                .avg( "avg_co2", "co2")
                .addGeneric(Data.class));
        return list;

    }

    public List<Data> getData7() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex( "idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy( "year", "year")
                .groupBy( "state_code", "state_code")
                .sum( "co2", "co2")
                .sum( "co2_catipa", "co2_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }

    public List<Data> getData8() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex( "idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy( "year", "year")
                .groupBy("state_code", "state_code")
                .sum( "co2", "co2")
                .sum( "co2_catipa", "co2_catipa")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.query(wrapper);
        return list;
    }

}
