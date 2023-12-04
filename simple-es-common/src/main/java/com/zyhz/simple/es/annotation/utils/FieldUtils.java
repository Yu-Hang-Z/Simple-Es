package com.zyhz.simple.es.annotation.utils;


import com.zyhz.simple.es.annotation.func.SFunction;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.*;
import java.util.Objects;
import java.util.function.Function;


/**
 * 核心 处理字段名称工具类
 * <p>
 * Copyright © 2021 xpc1024 All Rights Reserved
 **/
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FieldUtils {
    /**
     * 获取字段名称,如果有id,则将id转为_id
     *
     * @param func 列函数
     * @param <R>  泛型
     * @return 泛型
     */
    public static <R> String getFieldName(R func) {
        return getFieldNameNotConvertId(func);
    }

    /**
     * 获取字段名称,不转换id
     *
     * @param func 列函数
     * @param <R>  泛型
     * @return 泛型
     */
    public static <R> String getFieldNameNotConvertId(R func) {
        if (!(func instanceof SFunction)) {
            throw new RuntimeException("not support this type of column");
        }

        try {
            // 通过获取对象方法，判断是否存在该方法
            Method method = func.getClass().getDeclaredMethod("writeReplace");
            method.setAccessible(Boolean.TRUE);
            // 利用jdk的SerializedLambda 解析方法引用
            SerializedLambda serializedLambda = (SerializedLambda) method.invoke(func);
            String getter = serializedLambda.getImplMethodName();
            return resolveFieldName(getter);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }
    }




    /**
     * 获取字段名
     *
     * @param func 函数
     * @param <T>  泛型
     * @return 字段名称
     */
    public static <T> String val(Function<T, ?> func) {
        try {
            String getter;
            if (func instanceof Proxy) {
                InvocationHandler handler = Proxy.getInvocationHandler(func);
                Field field = handler.getClass().getDeclaredField("val$target");
                field.setAccessible(Boolean.TRUE);
                MethodHandle dmh = (MethodHandle) field.get(handler);
                Executable executable = MethodHandles.reflectAs(Executable.class, dmh);
                getter = executable.getName();
            } else {
                Method method = func.getClass().getDeclaredMethod("writeReplace");
                method.setAccessible(Boolean.TRUE);
                SerializedLambda serializedLambda = (SerializedLambda) method.invoke(func);
                getter = serializedLambda.getImplMethodName();

                // 添加调试信息
                System.out.println("Lambda type: " + func.getClass());
                System.out.println("SerializedLambda: " + serializedLambda);
            }
            return resolveFieldName(getter);
        } catch (ReflectiveOperationException e) {
            e.printStackTrace();
            throw new RuntimeException("no get method found!");
        }
    }


    /**
     * 处理获取字段名称
     *
     * @param getMethodName get方法的名字
     * @return 字段名称
     */
    public static String resolveFieldName(String getMethodName) {
        if (getMethodName.startsWith("get")) {
            getMethodName = getMethodName.substring(3);
        } else if (getMethodName.startsWith("is")) {
            getMethodName = getMethodName.substring(2);
        }
        // 小写第一个字母
        return firstToLowerCase(getMethodName);
    }

    /**
     * 将首字母小写
     *
     * @param param 参数
     * @return 首字母小写后的结果
     */
    private static String firstToLowerCase(String param) {
        if (Objects.isNull(param) || "".equals(param)) {
            return "";
        }
        return param.substring(0, 1).toLowerCase() + param.substring(1);
    }









}
