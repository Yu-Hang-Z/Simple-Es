package com.zyhz.simple.es.core.helper;

import com.zyhz.simple.es.annotation.IndexField;
import com.zyhz.simple.es.annotation.IndexName;
import com.zyhz.simple.es.annotation.utils.AnnotationUtils;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author by zhangyuhang
 * @Date 2023/11/22 09:37
 */
@Component
public class IndexEntityHelper implements ApplicationRunner {

    /**
     * 用于存储所有带有 @IndexName 注解的类的信息
     * key: 类的全限定名
     * value: Map，key 为字段名，value 为es字段的名
     */
    public static Map<String, Map<String, String>> indexInfoMap = new HashMap<>();

    /**
     * 用于存储所有带有 @IndexName 注解的类的信息
     * key: 类的全限定名
     * value: indexName
     */
    public static Map<String, String> indexNameMap = new HashMap<>();

    @Override
    public void run(ApplicationArguments args) throws Exception {
        // 扫描所有带有 @IndexName 注解的类
        // todo 为什么要扫描所有带有 @IndexName 注解的类？，如何扫描进去的？
        Set<Class<?>> classes = AnnotationUtils.getClassesWithAnnotation("", IndexName.class);
        for (Class<?> clazz : classes) {
            if (clazz.isAnnotationPresent(IndexName.class)) {
                String indexName = clazz.getAnnotation(IndexName.class).value();
                Map<String, String> fieldInfoMap = new HashMap<>();

                // 获取带有 @IndexField 注解的字段信息
                for (Field field : clazz.getDeclaredFields()) {
                    if (field.isAnnotationPresent(IndexField.class)) {
                        String fieldName = field.getName();
                        String indexFieldValue = field.getAnnotation(IndexField.class).value();
                        fieldInfoMap.put(fieldName, indexFieldValue);
                    }
                }

                // 将信息存储到全局的 Map 中
                indexInfoMap.put(clazz.getName(), fieldInfoMap);
                indexNameMap.put(clazz.getName(), indexName);
            }
        }

        // 在这里，你可以将 indexInfoMap 中的信息用于你的业务逻辑
        // 比如，可以将信息输出到日志中
        System.out.println("Index Information Map: " + indexInfoMap);
    }
}
