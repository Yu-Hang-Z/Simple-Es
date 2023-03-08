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

    @RequestMapping("getList1")
    public List<Data> getList1() {
        List<Data> list = new ArrayList<Data>();
        try {
            list = dataService.getData1();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return list;
    }

    @RequestMapping("getList2")
    public List<Data> getList2() {
        List<Data> list = new ArrayList<Data>();
        try {
            list = dataService.getData2();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return list;
    }

    @RequestMapping("getList3")
    public List<Data> getList3() {
        List<Data> list = new ArrayList<Data>();
        try {
            list = dataService.getData3();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return list;
    }

    @RequestMapping("getList4")
    public List<Data> getList4() {
        List<Data> list = new ArrayList<Data>();
        try {
            list = dataService.getData4();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return list;
    }
}
