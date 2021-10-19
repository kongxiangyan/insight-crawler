# grappler

周期性检索「中华人民共和国中央人民政府」网站指定页面的数据条目，根据标题判断是否是新数据，将新数据写入目标数据库中。

监测页面如下

- [首页 > 政策 > 最新](http://www.gov.cn/zhengce/zuixin.htm)
  可能在域名解析层面设置了访问限制，直接访问 IP 地址可以不设代理拿到数据（[IP 地址](http://182.18.80.137:80/zhengce/zuixin.htm)）。
- [太原市人民政府 > 政府信息公开 > 法定主动公开内容 > 国民经济和社会发展规划](http://www.taiyuan.gov.cn/fzlm/gkmlpt/zdgk/index.shtml?chan=25)
  很奇怪，这个没有访问限制，但数据是动态加载的，需要直接请求接口。
  [IP 地址](http://221.204.12.122:80/fzlm/gkmlpt/zdgk/index.shtml?chan=25)。
  [数据地址](http://taiyuan.gov.cn/intertidwebapp/govChanInfo/getDocuments?Index=1&pageSize=20&siteId=1&ChannelType=1&KeyWord=&KeyWordType=&chanId=25&order=1)。

运行 Docker 需要提供三个环境变量，分别是：

```bash
TC_SECRET_ID
TC_SECRET_KEY
TCB_ENV_ID
```
