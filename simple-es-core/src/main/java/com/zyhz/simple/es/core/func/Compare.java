package com.zyhz.simple.es.core.func;



import com.zyhz.simple.es.annotation.utils.FieldUtils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public interface Compare<Children,R> extends Serializable {

    default Children in(R column, Object val) {
        return in(true, FieldUtils.getFieldName(column), val);
    }

    default Children between(R field, Object from, Object to) {
        return between(true, FieldUtils.getFieldName(field), from, to);
    }

    default Children groupBy(R columns) {
        return groupBy(true, FieldUtils.getFieldName(columns), FieldUtils.getFieldName(columns));
    }

    default Children sum(R columns) {
        return sum(true, FieldUtils.getFieldName(columns), FieldUtils.getFieldName(columns));
    }

    default Children not(R columns, Object field) {
        return not(true, FieldUtils.getFieldName(columns), field);
    }

    default Children equals(R columns, Object field) {
        return equals(true, FieldUtils.getFieldName(columns), field);
    }

    default Children greaterThanOrEquals(R columns, Object field) {
        return greaterThanOrEquals(true, FieldUtils.getFieldName(columns), field);
    }

    default Children lessThan(R columns, Object field) {
        return lessThan(true, FieldUtils.getFieldName(columns), field);
    }

    default Children lessThanOrEquals(R columns, Object field) {
        return lessThanOrEquals(true, FieldUtils.getFieldName(columns), field);
    }

    default Children greaterThan(R columns, Object field) {
        return greaterThan(true, FieldUtils.getFieldName(columns), field);
    }

    default Children source(R... columns) {
        List<String> columnList = new ArrayList<>();
        for (R column : columns) {
            columnList.add(FieldUtils.getFieldName(column));
        }
        return source(true, columnList);
    }

    default Children sum(R... columns) {
        List<String> columnList = new ArrayList<>();
        for (R column : columns) {
            columnList.add(FieldUtils.getFieldName(column));
        }
        return sum(true, columnList);
    }



    Children in(boolean condition, String column, Object val);

    Children between(boolean condition, String field, Object from, Object to);

    Children groupBy(boolean condition, String columns, String field);

    Children sum(boolean condition, String columns, String field);

    Children sum(boolean condition, List<String> columns);

    Children not(boolean condition, String columns, Object field);

    Children equals(boolean condition, String columns, Object field);

    Children lessThan(boolean condition, String columns, Object field);

    Children lessThanOrEquals(boolean condition, String columns, Object field);

    Children greaterThan(boolean condition, String columns, Object field);

    Children greaterThanOrEquals(boolean condition, String columns, Object field);

    Children source(boolean condition, List<String> columns);
}
