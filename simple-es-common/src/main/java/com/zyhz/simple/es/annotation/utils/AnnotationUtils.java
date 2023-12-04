package com.zyhz.simple.es.annotation.utils;

import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import java.lang.annotation.Annotation;
import java.util.HashSet;
import java.util.Set;

/**
 * @author by zhangyuhang
 * @Date 2023/11/22 09:41
 */
public class AnnotationUtils {
    public static Set<Class<?>> getClassesWithAnnotation(String basePackage, Class<? extends Annotation> annotation) {
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(annotation));

        Set<Class<?>> classes = new HashSet<>();
        for (org.springframework.beans.factory.config.BeanDefinition bd : scanner.findCandidateComponents(basePackage)) {
            try {
                Class<?> clazz = Class.forName(bd.getBeanClassName());
                classes.add(clazz);
            } catch (ClassNotFoundException e) {
                // Handle exception as needed
                e.printStackTrace();
            }
        }
        return classes;
    }
}
