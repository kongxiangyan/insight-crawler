# Insights Grappler

周期性抓取官方发布的政策或通知，推送给订阅者。

## 监测目标

### 中央

- 中华人民共和国中央人民政府：[首页 > 政策 > 最新](http://www.gov.cn/zhengce/zuixin.htm)

  根据列表中标题后面的日期判断是否是新数据，将新数据写入目标数据库中。

### 山西

- [太原市人民政府 > 政府信息公开 > 法定主动公开内容 > 国民经济和社会发展规划](http://www.taiyuan.gov.cn/fzlm/gkmlpt/zdgk/index.shtml?chan=25)

  数据是动态加载的，需要模拟真实访问或直接请求接口，[数据接口地址](http://taiyuan.gov.cn/intertidwebapp/govChanInfo/getDocuments?Index=1&pageSize=20&siteId=1&ChannelType=1&KeyWord=&KeyWordType=&chanId=25&order=1)。

## 使用说明

### 运行

EXE 版本已经发布为 Docker Image，可以直接拉到本地运行。

```bash
# pull image
docker pull cigaret/insights-grappler:latest
# run container
docker run -dit -e TC_SECRET_ID=[YourSecretID] -e TC_SECRET_KEY=[YourSecretKey] -e TCB_ENV_ID=[YourEnvID] cigaret/insights-grappler:latest
```

运行有以下条件：

- 腾讯云云开发数据库，建立两个集合：
  - `GovStrategies`: 存储抓取到的数据条目
  - `Subscriptions`: 存储订阅通知的地址

```javascript
// GovStrategies Data Structure
const dataItem = {
  _id: "auto generate",
  grapTime: 1634550725,
  publishTime: 1625184000,
  title: "国务院办公厅关于加快发展保障性租赁住房的意见",
  url: "http://www.gov.cn/zhengce/content/2021-07/02/content_5622027.htm"
}
// Subscriptions Data Structure
// notifyType 有两个取值，取值不同的时候，url 接收到的消息具备不同的内容和格式：
//   - WORK_WEIXIN: 企业微信机器人 webhook url
//   - GENERAL: 新数据的 JSON 表示
const subscriber = {
  _id: "auto generate",
  notifyType: "WORK_WEIXIN" | "GENERAL",
  url: "https://qyapi.weixin.qq.com/cgi-bin/webhook/send?key=[YourKey]"
}
```

相关资料：

- [云开发 CloudBase - 腾讯云](https://cloud.tencent.com/product/tcb)
- [群机器人配置说明 - 企业微信 API 文档](https://work.weixin.qq.com/api/doc/90000/90136/91770)

### 开发

开发完成之后打包为镜像。

```bash
# build docker
docker build -t cigaret/insights-grappler:latest .
# push docker
docker push cigaret/insights-grappler:latest
```

## Author

- **Cigaret** - kcigaret@outlook.com

## License

This project is licensed under the **GPL-3.0** License - see the [LICENSE](LICENSE) file for details.
