package com.zyhz.simple.es.sample.controller;

import com.zyhz.simple.es.sample.entity.Data;
import com.zyhz.simple.es.sample.service.DataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


import java.io.IOException;

import java.util.ArrayList;
import java.util.List;

/**
 * @author by zhangyuhang
 * @Classname DataController
 * @Description TODO
 * @Date 2023/3/3 11:49
 */

@RestController
public class DataController {
    @Autowired
    private DataService dataService;

    @RequestMapping("getList")
    public List<Data> insert() {
        List<Data> list = new ArrayList<Data>();
        try {
            list = dataService.getData();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return list;
    }
}