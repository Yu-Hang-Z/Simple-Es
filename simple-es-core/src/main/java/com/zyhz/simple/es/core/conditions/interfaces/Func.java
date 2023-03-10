package com.zyhz.simple.es.core.conditions.interfaces;

import java.io.Serializable;

public interface Func<Children> extends Serializable {

    Children ne(boolean condition, String columns, Object field);

    Children eq(boolean condition, String columns, Object field);

    Children in(boolean condition, String column, Object val);

    Children notIn(boolean condition, String column, Object val);

    Children between(boolean condition, String field, Object from, Object to);

    Children notBetween(boolean condition, String field, Object from, Object to);

    Children gt(boolean condition, String field, Object val);

    Children ge(boolean condition, String field, Object val);

    Children lt(boolean condition, String field, Object val);

    Children le(boolean condition, String field, Object val);



    Children groupBy(boolean condition, String columns, String field);

    Children sum(boolean condition, String columns, String field);

    Children min(boolean condition, String columns, String field);

    Children max(boolean condition, String columns, String field);

    Children count(boolean condition, String columns, String field);

    Children avg(boolean condition, String columns, String field);


}
