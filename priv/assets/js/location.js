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

    /**
     * 加载用户设备列表
     * 只加载当前登录账号有权限的、状态为非禁用的设备
     */
    function loadUserDevices() {
        $.ajax({
            url: '/device/user_devices',
            type: 'GET',
            success: function(response) {
                if (response && response.length > 0 && response[0].Alert) {
                    showWarningToast(response[0].Alert);
                } else {
                    let html = '<option value="" selected>全部设备</option>';

                    // 检查响应格式并提取设备数据
                    let deviceData = [];
                    if (Array.isArray(response)) {
                        deviceData = response;
                    } else if (response && response.data && Array.isArray(response.data)) {
                        deviceData = response.data;
                    }

                    if (deviceData.length === 0) {
                        showWarningToast("未找到可用设备！");
                    }

                    // 生成设备选项，只显示设备号
                    deviceData.forEach(function(device) {
                        html += '<option value="' + device.deviceno + '">' + device.deviceno + '</option>';
                    });

                    $('#deviceSelect').html(html);
                }
            },
            error: function(xhr, _, error) {
                try {
                    const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                    if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                        showWarningToast(errorResponse[0].Alert);
                    } else {
                        showWarningToast("加载设备列表失败：" + error);
                    }
                } catch (e) {
                    showWarningToast("加载设备列表失败！");
                }
            }
        });
    }

    /**
     * 显示警告提示
     * @param {string} message - 提示信息
     */
    function showWarningToast(message) {
        const toastElList = [].slice.call(document.querySelectorAll('.toast'));
        const toastList = toastElList.map(function (toastEl) {
            const toastBodyEl = toastEl.querySelector('.toast-body');
            toastBodyEl.textContent = message;
            return new bootstrap.Toast(toastEl);
        });
        toastList.forEach(toast => toast.show());
    }

    /**
     * 加载位置数据
     * @param {Function} callback - 回调函数，用于处理返回的位置数据
     */
    function loadLocationData(callback) {
        // 获取查询参数
        const startTime = $('#starttime').val();
        const endTime = $('#endtime').val();
        const deviceNo = $('#deviceSelect').val() || '';

        // 验证参数
        if (!startTime || !endTime) {
            showWarningToast("请选择开始和结束时间！");
            callback([]);
            return;
        }

        // 构建查询参数
        const searchParams = {
            startTime: startTime,
            endTime: endTime,
            deviceNo: deviceNo
        };

        // 发送请求获取位置数据
        $.ajaxSetup({async:false});
        $.getJSON('/location', searchParams, function (mapsData) {
            if (mapsData && mapsData.length > 0 && mapsData[0].Alert) {
                showWarningToast(mapsData[0].Alert);
                callback([]); // 如果有警报，回调函数返回空数组
            } else if (Array.isArray(mapsData) && mapsData.length > 0) {
                callback(mapsData); // 回调函数返回地图数据
            } else {
                showWarningToast("未查询到轨迹数据！");
                callback([]);
            }
        }).fail(function(_, _, error) {
            showWarningToast("查询轨迹数据失败：" + error);
            callback([]);
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
        // 初始化日期时间选择器
        $('#starttime').datetimepicker({
            format: 'Y-m-d H:i:s',
            step: 10,
            defaultDate: new Date(new Date().setHours(0, 0, 0, 0)), // 默认为今天的开始时间
            defaultTime: '00:00:00'
        });
        $('#endtime').datetimepicker({
            format: 'Y-m-d H:i:s',
            step: 10,
            defaultDate: new Date(), // 默认为当前时间
            defaultTime: new Date().toTimeString().slice(0, 8)
        });

        // 设置默认时间值
        const now = new Date();
        const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
        $('#starttime').val(formatDateTime(today));
        $('#endtime').val(formatDateTime(now));

        // 加载用户设备列表
        loadUserDevices();

        // 开始回放按钮点击事件
        $('#start').click(function() {
            if (!validateInputs()) return;

            loadLocationData(function (data){
                reloadCarReplay(data);
            });
        });

        // 查询按钮点击事件（如果页面上有这个按钮）
        $('#searchLocation').click(function() {
            if (!validateInputs()) return;

            loadLocationData(function (data){
                reloadCarReplay(data);
            });
        });

        // 清空按钮点击事件
        $('#cleanLocation').click(function() {
            $('#starttime').val('');
            $('#endtime').val('');
            $('#deviceSelect').val('');
        });

        // 设备选择变更事件
        $('#deviceSelect').change(function() {
            // 如果有其他需要根据设备选择更新的UI元素，可以在这里处理
        });
    })

    /**
     * 验证输入参数
     * @returns {boolean} 验证结果
     */
    function validateInputs() {
        const startTime = $('#starttime').val();
        const endTime = $('#endtime').val();

        if (!startTime || !endTime) {
            showWarningToast("请选择开始和结束时间！");
            return false;
        }

        // 可以添加更多验证逻辑，如时间范围检查等

        return true;
    }

    /**
     * 格式化日期时间
     * @param {Date} date - 日期对象
     * @returns {string} - 格式化后的日期时间字符串
     */
    function formatDateTime(date) {
        return date.getFullYear() + '-' +
               padZero(date.getMonth() + 1) + '-' +
               padZero(date.getDate()) + ' ' +
               padZero(date.getHours()) + ':' +
               padZero(date.getMinutes()) + ':' +
               padZero(date.getSeconds());
    }

    /**
     * 数字补零
     * @param {number} num - 数字
     * @returns {string} - 补零后的字符串
     */
    function padZero(num) {
        return num < 10 ? '0' + num : num;
    }

})
.catch((e) => {
    console.error(e); // 加载错误提示
});

