package com.zyhz.simple.es.sample.service;

import com.zyhz.simple.es.core.base.BasedQueryES;
import com.zyhz.simple.es.core.conditions.QueryWrapper;
import com.zyhz.simple.es.sample.entity.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.io.IOException;
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
                .setQuerySize(true, 100)
                .setQueryFrom(true, 0)
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.baseQuery(warper);
        return list;

    }

    public List<Data> getData2() throws IOException {
        QueryWrapper wrapper = QueryWrapper.create()
                .setQueryIndex(true, "idx_emis_server_aggregation_pop_year_7754_v1")
                .groupBy(true, "state_code", "state_code")
                .sum(true, "co2", "co2")
                .addGeneric(Data.class);
        List<Data> list =  basedQueryES.aggregationQuery(wrapper);
        return list;
    }
}
