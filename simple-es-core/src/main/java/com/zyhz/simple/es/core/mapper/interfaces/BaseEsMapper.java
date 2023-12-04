package com.zyhz.simple.es.core.mapper.interfaces;



import com.zyhz.simple.es.core.wrapper.interfaces.Wrapper;

import java.io.IOException;
import java.util.List;

/**
 * @author by zhangyuhang
 * @Date 2023/11/21 16:14
 */
public interface BaseEsMapper<T> {
    Class<T> getEntityClass();

    List<T> selectList(Wrapper<T> wrapper) throws IOException;

    List<T> selectAggregateList(Wrapper<T> wrapper) throws IOException;

}
