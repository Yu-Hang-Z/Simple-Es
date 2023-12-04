package com.zyhz.simple.es.core.wrapper;

/**
 * @author by zhangyuhang
 * @Date 2023/11/21 17:37
 */
public class EsWrappers {

    public static <T> LambdaEsQueryWrapper<T> lambdaQuery(Class<T> entityClass) {
        return new LambdaEsQueryWrapper<T>(entityClass);
    }
}
