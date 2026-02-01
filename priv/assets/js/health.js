/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-01 13:24
 * Updated : 2025-11-24
 *
 * Module : health.js
 *
 */

let healthChart = null;

function translateColumnNames(columnName) {
    const translations = i18nHealth.columnName[defaultLanguage];
    return translations[columnName] || columnName;
}

function translateSleepType(columnName) {
    const translations = i18nHealth.sleepType[defaultLanguage];
    return translations[columnName] || columnName;
}

// 数据类型配置
const dataTypeConfig = {
    'Steps': { name: '步数', unit: '步', color: '#ff6b6b' },
    'HeartRate': { name: '心率', unit: 'bpm', color: '#ee6666' },
    'Temperature': { name: '体温', unit: '°C', color: '#fac858' },
    'Pressure': { name: '血压', unit: 'mmHg', color: '#91cc75' },
    'Sleep': { name: '睡眠', unit: '小时', color: '#ffb3ba' },
    'Battery': { name: '电量', unit: '%', color: '#ff8a8a' }
};

// 初始化图表
function initChart() {
    const chartDom = document.getElementById('health-chart');
    if (healthChart) {
        healthChart.dispose();
    }
    healthChart = echarts.init(chartDom);

    // 设置初始空状态
    healthChart.setOption({
        title: {
            text: '健康数据趋势图',
            left: 'center',
            top: 10,
            textStyle: {
                fontSize: 16,
                fontWeight: 'normal'
            }
        },
        grid: {
            left: '3%',
            right: '4%',
            bottom: '3%',
            containLabel: true
        },
        graphic: {
            type: 'text',
            left: 'center',
            top: 'middle',
            style: {
                text: '请选择时间段并点击查询按钮',
                fontSize: 14,
                fill: '#999'
            }
        }
    });
}

// 加载健康数据并渲染图表
function loadHealthData(startTime, endTime) {
    const searchParams = {
        dataType: '0',  // 固定查询所有类型
        startTime: startTime,
        endTime: endTime
    };

    $.getJSON('/health', searchParams, function (response) {
        if (response && response.length > 0 && response[0].Alert) {
            showWarningToast(response[0].Alert);
            renderEmptyChart();
            return;
        }

        if (!response || !response.data || response.data.length === 0) {
            showWarningToast("此时间段内无健康数据!");
            renderEmptyChart();
            return;
        }

        // 渲染图表
        renderHealthChart(response);
    }).fail(function () {
        showWarningToast("数据加载失败");
        renderEmptyChart();
    });
}

// 渲染空图表
function renderEmptyChart() {
    if (!healthChart) {
        initChart();
    }

    healthChart.setOption({
        title: {
            text: '健康数据趋势图',
            left: 'center',
            top: 10
        },
        graphic: {
            type: 'text',
            left: 'center',
            top: 'middle',
            style: {
                text: '暂无数据',
                fontSize: 14,
                fill: '#999'
            }
        },
        xAxis: {},
        yAxis: {},
        series: []
    });
}

// 渲染健康数据图表
function renderHealthChart(response) {
    if (!healthChart) {
        initChart();
    }

    const data = response.data;
    const columns = response.columns;

    // 提取时间轴数据
    const timeColumn = columns.find(col => col.includes('Time') || col.includes('Date'));
    if (!timeColumn) {
        showWarningToast("数据格式错误:缺少时间字段");
        return;
    }

    const times = data.map(item => item[timeColumn]);

    // 构建系列数据
    const series = [];
    const legend = [];

    // 遍历所有非时间列
    columns.forEach(col => {
        if (col === timeColumn || col.includes('Type')) {
            return;
        }

        // 查找匹配的数据类型配置
        let config = null;
        let displayName = translateColumnNames(col);

        for (const [key, value] of Object.entries(dataTypeConfig)) {
            if (col.includes(key)) {
                config = value;
                break;
            }
        }

        legend.push(displayName);

        series.push({
            name: displayName,
            type: 'line',
            smooth: true,
            symbol: 'circle',
            symbolSize: 6,
            itemStyle: {
                color: config ? config.color : undefined
            },
            lineStyle: {
                width: 2
            },
            data: data.map(item => {
                let value = item[col];
                // 处理睡眠类型翻译
                if (col === 'SleepType') {
                    value = translateSleepType(value);
                }
                return value;
            })
        });
    });

    // 设置图表选项
    const option = {
        title: {
            text: '健康数据趋势图',
            left: 'center',
            top: 10,
            textStyle: {
                fontSize: 16,
                fontWeight: 'normal'
            }
        },
        tooltip: {
            trigger: 'axis',
            axisPointer: {
                type: 'cross'
            }
        },
        legend: {
            data: legend,
            top: 40,
            left: 'center',
            type: 'scroll'
        },
        grid: {
            left: '3%',
            right: '4%',
            bottom: '15%',
            top: 80,
            containLabel: true
        },
        xAxis: {
            type: 'category',
            boundaryGap: false,
            data: times,
            axisLabel: {
                rotate: 45,
                formatter: function (value) {
                    // 格式化日期显示
                    if (value.length > 16) {
                        return value.substring(5, 16);
                    }
                    return value;
                }
            }
        },
        yAxis: {
            type: 'value',
            axisLabel: {
                formatter: '{value}'
            }
        },
        series: series,
        dataZoom: [
            {
                type: 'inside',
                start: 0,
                end: 100
            },
            {
                start: 0,
                end: 100,
                height: 20,
                bottom: 10
            }
        ]
    };

    healthChart.setOption(option, true);
}

// 导出健康数据为Excel文件
function exportHealthData() {
    const startTime = $('#starttime').val();
    const endTime = $('#endtime').val();

    if (!startTime || !endTime) {
        showWarningToast("请选择开始和结束时间");
        return;
    }

    const searchParams = {
        dataType: '0',
        startTime: startTime,
        endTime: endTime
    };

    $.getJSON('/health', searchParams, function (response) {
        if (!response || !response.data || response.data.length === 0) {
            showWarningToast("无数据可导出");
            return;
        }

        // 创建工作簿
        const wb = XLSX.utils.book_new();
        const ws = XLSX.utils.json_to_sheet(response.data);

        // 添加工作表到工作簿
        XLSX.utils.book_append_sheet(wb, ws, "健康数据");

        // 生成文件名
        const fileName = `健康数据_${startTime.replace(/[\/:]/g, '')}_${endTime.replace(/[\/:]/g, '')}.xlsx`;

        // 导出Excel文件
        XLSX.writeFile(wb, fileName);
        showSuccessToast("导出成功");
    }).fail(function () {
        showWarningToast("导出失败");
    });
}

$(document).ready(function () {
    // 初始化图表
    initChart();

    // 默认加载数据
    loadHealthData(defaultStartTime, defaultEndTime);

    // 查询按钮点击事件
    $('#searchHealth').click(function () {
        const startTime = $('#starttime').val();
        const endTime = $('#endtime').val();

        if (!startTime || !endTime) {
            showWarningToast("请选择开始和结束时间");
            return;
        }

        loadHealthData(startTime, endTime);
    });

    // 清空按钮点击事件
    $('#cleanHealth').click(function () {
        $('input[type="text"]').val('');
    });

    // 刷新按钮点击事件
    $('#refresh-health-btn').click(function () {
        const startTime = $('#starttime').val() || defaultStartTime;
        const endTime = $('#endtime').val() || defaultEndTime;
        loadHealthData(startTime, endTime);
        showSuccessToast("数据已刷新");
    });

    // 导出按钮点击事件
    $('#export-health-btn').click(function () {
        exportHealthData();
    });

    // 窗口大小改变时重新调整图表
    window.addEventListener('resize', function () {
        if (healthChart) {
            healthChart.resize();
        }
    });
});
