/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-12 11:28
 *
 * Module : amaploader.js
 *
 */

window.ALoader = window.ALoader || {};

window._AMapSecurityConfig = {
    securityJsCode: "641af2cc3789991d5408e3429e29bb11",
};

AMapLoader.load({
    key: "8ed6fc7322bad71471c034af8b681cb6", //申请好的 Web 端开发 Key，首次调用 load 时必填
    version: "2.0", //指定要加载的 JS API 的版本，缺省时默认为 1.4.15
    // 加载插件列表
    plugins: ["AMap.Scale", // 比例尺
        "AMap.ToolBar", // 缩放
        "AMap.Geolocation", // 定位获取
        "AMap.Marker", // 标记点绘制
        "AMap.Pixel", // 像素点
        "AMap.Polyline", // 线
        "AMap.LngLat", // 经纬度
        "AMap.convertFrom", // 坐标系转换
        "AMap.GraspRoad", // 轨迹纠偏
    ],
    AMapUI: {
      //是否加载 AMapUI，缺省不加载
      version: "1.1", //AMapUI 版本
      plugins: ["overlay/SimpleMarker"], //需要加载的 AMapUI ui 插件
    },
    Loca: {
      //是否加载 Loca， 缺省不加载
      version: "2.0", //Loca 版本
    },
})

.then((AMap) => {
    // 创建地图实例
    ALoader.mapwong = new AMap.Map("mapContainer", {  //"container"为 <div> 容器的 id
        viewMode: '2D', //默认使用 2D 模式
        zoom: 11, //地图级别
        center: [120.527112, 36.411864],
        mapStyle: "amap://styles/fresh", //设置地图的显示样式
    });

    // 设置地图中心点为当前位置
    let currentCenter = ALoader.mapwong.getCenter().toJSON();
    ALoader.mapwong.setZoomAndCenter(13, currentCenter, false, 2000);

    ALoader.mapwong.addControl(new AMap.Scale()); //添加比例尺
    ALoader.mapwong.addControl(new AMap.ToolBar({position: 'LT'})); //添加缩放工具条
    ALoader.mapwong.addControl(new AMap.Geolocation()); //添加获取/回到当前定位
    // mapwong.addControl(new AMap.MapType({position: { left: 5, top: 20 }})); //添加图层切换
})

.catch((e) => {
  console.error(e); //加载错误提示
});
