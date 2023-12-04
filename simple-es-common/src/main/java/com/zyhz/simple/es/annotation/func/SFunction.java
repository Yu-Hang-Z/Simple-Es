package com.zyhz.simple.es.annotation.func;

import java.io.Serializable;

/**
 * @author by zhangyuhang
 * @Classname SFunction
 * @Description TODO
 * @Date 2023/11/17 10:44
 */
@FunctionalInterface
public interface SFunction<T, R> extends Serializable {
    R apply(T t);
}
