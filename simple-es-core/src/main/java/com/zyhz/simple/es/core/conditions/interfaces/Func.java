package com.zyhz.simple.es.core.conditions.interfaces;

import java.io.Serializable;

public interface Func<Children> extends Serializable {

    Children not(boolean condition, String columns, Object field);

    Children equals(boolean condition, String columns, Object field);

    Children in(boolean condition, String column, Object val);

    Children between(boolean condition, String field, Object from, Object to);



    Children groupBy(boolean condition, String columns, String field);

    Children sum(boolean condition, String columns, String field);


}
