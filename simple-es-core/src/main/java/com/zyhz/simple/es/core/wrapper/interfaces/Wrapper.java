package com.zyhz.simple.es.core.wrapper.interfaces;


import com.zyhz.simple.es.core.conditions.interfaces.BaseCondition;
import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;

import java.util.List;


/**
 * @author by zhangyuhang
 * @Date 2023/11/21 16:04
 */
public class Wrapper<T> implements Cloneable {

    @Getter
    @Setter
    protected Integer pageSize = 50;

    @Getter
    @Setter
    protected Integer page = 1;

    /**
     * 实体对象
     */
    protected T entity;

    protected Class<T> entityClass;

    /**
     * 查询条件,使用队列存储
     */
    protected List<BaseCondition> basedQueryConditions;





    @SneakyThrows
    protected Wrapper<T> clone() {
        return (Wrapper<T>) super.clone();
    }
}
