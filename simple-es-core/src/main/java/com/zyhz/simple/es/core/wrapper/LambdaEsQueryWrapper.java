package com.zyhz.simple.es.core.wrapper;


import com.zyhz.simple.es.annotation.func.SFunction;
import com.zyhz.simple.es.core.wrapper.interfaces.AbstractWrapper;

/**
 * @author by zhangyuhang
 * @Date 2023/11/21 15:36
 */
public class LambdaEsQueryWrapper<T> extends AbstractWrapper<T, SFunction<T,?>, LambdaEsQueryWrapper<T>> {

    public LambdaEsQueryWrapper() {
        this(null);
    }

    public LambdaEsQueryWrapper(Class<T> entityClass) {
        super.initNeed();
        super.setEntityClass(entityClass);
    }
}
