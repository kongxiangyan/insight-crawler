# grappler

项目采用云开发数据库（CloudBase Database）作为数据存储设施，但 CloudBase Database 没有提供 Haskell SDK，故 grappler 选择使用 CloudBase Database OPEN API 进行数据库操作，顺便把腾讯云产品访问的签名和鉴权机制使用 Haskell 实现了一遍。

- [OPEN API - API 参考 | 云开发 CloudBase](https://docs.cloudbase.net/api-reference/openapi/introduction)
- [访问 CloudBase 云数据库 - 云托管 CloudBase Run | 腾讯云文档](https://cloud.tencent.com/document/product/1243/49232)
