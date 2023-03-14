
## 项目介绍

Simple-Es是一个快速处理聚合查询ES的工具包，可以向使用Mybatis-plus一样方便快捷的处理聚合查询相关问题。

## 项目结构

```
simple-Es -- 父工程
├── simple-es-annotation -- 注解模块
├── simple-es-boot-starter -- 整合Spring Boot
├── simple-es-common -- 基础模块
├── simple-es-core -- 核心
├── simple-es-extension -- 扩展
├── simple-es-parent -- 包管理
├── simple-es-sample -- 案例
└── simple-es-test -- 测试
```

## 技术选型

|        技术        |         说明         |
|:----------------:|:------------------:|
| Spring Framework |    Spring 核心框架     |
|  Elasticsearch   | Elasticsearch 搜索引擎 |
|      Lombok      |  Lombok 简化对象封装工具   |
|      Hutool      |     Hutool 工具类     |
|     Swagger      |    Swagger 接口文档    |
|     Mybatis      |     Mybatis ORM框架     |
|     Mybatis-Plus |   Mybatis-Plus ORM框架   |
|     HikariCP     |     HikariCP 连接池     |
|      Log4j2      |      Log4j2 日志框架     |




## 入门案例


### 1.引入依赖

```xml 
<dependency>
    <groupId>com.github.simple</groupId>
    <artifactId>simple-es-boot-starter</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2.配置文件

```yaml
spring:
  elasticsearch:
    rest:
      uris: http://localhost:9200
```
### 3.编写实体类

```java
@Data
@Document(indexName = "test", type = "test")
public class TestEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    private String id;

    private String name;

    private Integer age;

    private String address;

    private String phone;

    private String email;

    private String remark;

    private Date createTime;

    private Date updateTime;

}
```
### 4.编写Mapper接口

```java
@EsRepository
public interface TestMapper extends BaseMapper<TestEntity> {

}
```
## MySQL Simple-Es and Es syntax comparison table
|    MySQL    | 	Simple-Es |    Es-DSL/Es java api     |
|:-----------:|:----------:|:-------------------------:|
|      =      |     eq     |         termQuery         |
|     !=      |     ne     |    mustNot(termQuery)     |
|      >      |     gt     |        rangeQuery         |
|     >=      |     ge     |        rangeQuery         |
|      <      |     lt     |        rangeQuery         |
|     <=      |     le     |        rangeQuery         |
|     in      |     in     |        termsQuery         |
|   not in    |   notIn    |    mustNot(termsQuery)    |
|    like     |    like    |       wildcardQuery       |
|  not like   |  notLike   |  mustNot(wildcardQuery)   |
|   is null   |   isNull   |   mustNot(existsQuery)    |
| is not null | isNotNull  |        existsQuery        |
|   between   |  between   |        rangeQuery         |
| not between | notBetween |    mustNot(rangeQuery)    |
|  group by   |  groupBy   | AggregationBuilders.terms |
|  order by   |  orderBy   |  SortBuilders.fieldSort   |
|    count    |   count    | AggregationBuilders.count |
|     sum     |    sum     |  AggregationBuilders.sum  |
|     avg     |    avg     |  AggregationBuilders.avg  |
|     max     |    max     |  AggregationBuilders.max  |
|     min     |    min     |  AggregationBuilders.min  |




## 赞赏支持

开源项目不易，若此项目能得到你的青睐，那么你可以赞赏支持作者持续开发与维护。

- 服务器的费用也是一笔开销
- 编写更完备的文档教程
- 一杯咖啡

![]()

## 免责声明

用户使用本系统从事任何违法违规的事情，一切后果由用户自行承担，作者不承担任何责任。
