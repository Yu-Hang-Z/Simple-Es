package com.zyhz.simple.es.sample;

import com.sun.jdi.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan(basePackages = {"com.zyhz.simple.*"})
public class SimpleEsSampleApplication {

    public static void main(String[] args) {
        SpringApplication.run(SimpleEsSampleApplication.class, args);
    }

}
