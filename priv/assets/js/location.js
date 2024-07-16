/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-07 11:29:20
 *
 * Module : location.js
 *
 */


// 引入高德地图加载器
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
        // "AMap.convertFrom", // 坐标系转换
        // "AMap.GraspRoad", // 轨迹纠偏
        "AMap.moveAnimation", // 覆盖物添加
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
    const carSpeed=100;

    function loadLocationData(callback) {
        // API获取定位信息
        const searchParams = {
            startTime: $('#starttime').val(),
            endTime: $('#endtime').val()
        };
        $.ajaxSetup({async:false});
        $.getJSON('/data/location', searchParams, function (mapsData) {
            // console.log("mapsData: ", JSON.stringify(mapsData));
            if (mapsData && mapsData.length > 0 && mapsData[0].Alert) {
                const toastElList = [].slice.call(document.querySelectorAll('.toast'));
                const toastList = toastElList.map(function (toastEl) {
                    const toastBodyEl = toastEl.querySelector('.toast-body');
                    toastBodyEl.textContent = mapsData[0].Alert;
                    return new bootstrap.Toast(toastEl);
                });
                toastList.forEach(toast => toast.show());
                callback([]); // 如果有警报，回调函数返回空数组
            } else {
                callback(mapsData); // 否则，回调函数返回地图数据
            }
        });
    }

    let mapwong = new AMap.Map("mapContainer", {  //"container"为 <div> 容器的 id
    viewMode: '2D', //默认使用 2D 模式
    resizeEnable: true,
    zoom: 17, //地图级别
    center: [120.527112, 36.411864],
    mapStyle: "amap://styles/fresh", //设置地图的显示样式
    });

    // 设置地图中心点为当前位置
    let currentCenter = mapwong.getCenter().toJSON();
    mapwong.setZoomAndCenter(13, currentCenter, false, 2000);
    mapwong.addControl(new AMap.Scale()); //添加比例尺
    mapwong.addControl(new AMap.ToolBar({position: 'LT'})); //添加缩放工具条
    mapwong.addControl(new AMap.Geolocation()); //添加获取/回到当前定位

    function reloadCarReplay(linePath) {
    AMap.plugin('AMap.MoveAnimation', function(){
        // 初始化车辆位置
        let carStart = [];
        if(linePath.length>0) {
           carStart = linePath[0]
        } else {
           carStart = [120.527112, 36.411864]
        }

        // 创建小汽车marker
        let carMarker = new AMap.Marker({
            map: mapwong,
            position: carStart,
            icon: "/assets/img/car.png",
            // offset: new AMap.Pixel(-13, -26),
        });

        // 创建跟速度信息展示框
        let carWindow = new AMap.InfoWindow({
            offset: new AMap.Pixel(6, -25),
            autoMove: true
        });

        // 生成路线
        let polyline;
        let passedPolyline;
        if (linePath.length > 0) {
            polyline = new AMap.Polyline({
                map: mapwong,
                path: linePath,
                showDir: true,
                strokeColor: '#28F',
                // strokeOpacity: 0.8,
                strokeWeight: 6,
                // strokeStyle: 'solid'
            });

            passedPolyline = new AMap.Polyline({
                map: mapwong,
                strokeColor: '#AF5',
                // strokeOpacity: 0.8,
                strokeWeight: 6,
            });

            // 回放监听
            carMarker.on('moving', function (e) {
                passedPolyline.setPath(e.passedPath);
                mapwong.setCenter(e.target.getPosition(),true);
                let lastLocation = e.passedPath[e.passedPath.length - 1];
                carWindow.setPosition(lastLocation);
                carWindow.setContent("当前经纬度: " + e.target.getPosition());
            });

            // 打开回放窗口
            carWindow.open(mapwong, carMarker.getPosition());

            // 地图自适应缩放
            mapwong.setFitView();

            // 开启回放动画
            carMarker.moveAlong(linePath, {
                // 每一段的时长
                duration: 10,//可根据实际采集时间间隔设置
                // JSAPI2.0 是否延道路自动设置角度在 moveAlong 里设置
                autoRotation: true,
            });

            $('#pause').click(function() {
            carMarker.pauseMove();
            });

            $('#resume').click(function() {
                carMarker.resumeMove();
            });

            $('#stop').click(function() {
                carMarker.stopMove();
                carWindow.close();
                passedPolyline.destroy();
                polyline.destroy();
                carMarker.destroy();
                linePath = [];
            });

        } else {
            const toastElList = [].slice.call(document.querySelectorAll('.toast'));
            const toastList = toastElList.map(function (toastEl) {
                const toastBodyEl = toastEl.querySelector('.toast-body');
                toastBodyEl.textContent = "此时间段内无定位数据！";
                return new bootstrap.Toast(toastEl);
            })
            toastList.forEach(toast => toast.show());
        }

    });
}

    $(document).ready(function() {

        $('#start').click(function() {
            loadLocationData(function (data){
                reloadCarReplay(data);
            });

        });

    })

})
.catch((e) => {
    console.error(e); // 加载错误提示
});

