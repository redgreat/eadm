/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-15 08:41:01
 *
 * Module : finance.js
 *
 */

function translateColumnNames(columnName) {
  const translations = i18nFinance.columnName[defaultLanguage];
  return translations[columnName] || columnName;
}

function translatesourceType(columnName) {
  const translations = i18nFinance.sourceType[defaultLanguage];
  return translations[columnName] || columnName;
}

// 导出财务数据为Excel文件
function exportFinanceData() {
    const sourceType = $('#sourceType').val();
    const inorOut = $('#inorOut').val();
    const startTime = $('#starttime').val();
    const endTime = $('#endtime').val();

    if (!startTime || !endTime) {
        showWarningToast("请选择开始和结束时间");
        return;
    }

    // 获取表格数据
    const table = $('#table-finance').DataTable();
    const data = table.data().toArray();
    const headers = [];

    // 获取表头
    table.columns().every(function() {
        headers.push(this.header().textContent);
    });

    if (data.length === 0) {
        showWarningToast("无数据可导出");
        return;
    }

    // 创建工作簿
    const wb = XLSX.utils.book_new();
    const ws = XLSX.utils.json_to_sheet(data, {header: headers});

    // 添加工作表到工作簿
    XLSX.utils.book_append_sheet(wb, ws, "财务数据");

    // 生成文件名
    let sourceTypeText = "全部";
    if (sourceType === "1") sourceTypeText = "支付宝";
    else if (sourceType === "2") sourceTypeText = "微信";
    else if (sourceType === "3") sourceTypeText = "青岛银行";
    else if (sourceType === "4") sourceTypeText = "中国银行";

    let inorOutText = "全部";
    if (inorOut === "1") inorOutText = "收入";
    else if (inorOut === "2") inorOutText = "支出";
    else if (inorOut === "3") inorOutText = "其他";

    const fileName = `财务数据_${sourceTypeText}_${inorOutText}_${startTime.replace(/[\/:]/g, '')}_${endTime.replace(/[\/:]/g, '')}.xlsx`;

    // 导出Excel文件
    XLSX.writeFile(wb, fileName);
}

function loadFinanceData(sourceType, inorOut, startTime, endTime) {
    const searchParams = {
        sourceType: sourceType,
        inorOut: inorOut,
        startTime: startTime,
        endTime: endTime
    };

    let dynamicColumns = [];
    let dynamicDatas = [];

    $.getJSON('/finance', searchParams, function (response) {

        function buildDynamicData(response) {
            response.columns.forEach(function (column) {
                let dynamicColumn = {};
                dynamicColumn['data'] = column;
                dynamicColumn['title'] = translateColumnNames(column);
                dynamicColumn['className'] = "dataTables-column";
                dynamicColumns.push(dynamicColumn);
                if (column === "SourceType") {
                    response.data.forEach(function (rowData) {
                    rowData["SourceType"] = translatesourceType(rowData["SourceType"]);
                    });
                }
            });
            dynamicColumns.push({"data": "Action", "title": "操作", "className": "dataTables-column"});
            dynamicDatas = response.data;
        }

        if (response && response.length > 0 && response[0].Alert) {
            showWarningToast(response[0].Alert);
        }
        else if (response && response.data.length === 0) {
            showWarningToast("此时间段内无财务数据！");
            response.columns.forEach(function (column) {
                let dynamicColumn = {};
                dynamicColumn['data'] = column;
                dynamicColumn['title'] = translateColumnNames(column);
                dynamicColumn['className'] = "dataTables-column";
                dynamicColumns.push(dynamicColumn);
            });
        }
        else {
            buildDynamicData(response);
        }
        //#TODO 调整为固定表格列，接口返回JSON数据后直接渲染
        $('#table-finance').DataTable().destroy();
        $('#table-finance').empty();
        $('#table-finance').DataTable({
            // lengthChange: true,  //是否允许用户改变表格每页显示的记录数
            // bStateSave: true,  //记录cookie
            columnDefs: dynamicColumns && dynamicColumns.length ? [
                {
                    targets: -1,
                    render: function (data, type, full, meta) {
                        return `
                    <button class="btn btn-outline-danger btn-rounded delete-btn">
                      <i class="fas fa-trash"></i>
                    </button>
                `;
                    }
                }
            ] : [],
            destroy: true, // 销毁重新渲染
            columns: dynamicColumns,
            data: dynamicDatas,
            responsive: true,
            info: true, // 是否显示左下角分页信息
            processing: true,  //是否显示处理状态(排序的时候，数据很多耗费时间长的话，也会显示这个)
            orderMulti: true,  //启用多列排序
            ordering: true,  //使用排序
            paging: true,  //是否分页
            pageLength: 10, //每页默认行数
            lengthChange: false, //是否可以改变每页显示的记录数
            pagingType: "full_numbers",  //除首页、上一页、下一页、末页四个按钮还有页数按钮
            searching: false,  //是否开始本地搜索
            stateSave: true,  //刷新时是否保存状态
            // autoWidth: true,  //自动计算宽度
            deferRender: true, // 延迟渲染
            language: {
                // decimal: "",//小数的小数位符号  比如“，”作为数字的小数位符号
                // infoFiltered: "(从 _MAX_ 条记录过滤)",//当表格过滤的时候，将此字符串附加到主要信息
                // infoPostFix: "",//在摘要信息后继续追加的字符串
                // search: "搜索",//用来描述搜索输入框的字符串
                // zeroRecords: "没有找到",//当没有搜索到结果时，显示
                info: "当前 _START_ 条到 _END_ 条 共 _TOTAL_ 条",//左下角的信息，变量可以自定义，到官网详细查看
                infoEmpty: "无记录",//当没有数据时，左下角的信息
                emptyTable: "未查到数据",//当表格为空时，表格中信息
                thousands: ",",//千分位分隔符
                lengthMenu: "每页 _MENU_ 条记录",//用来描述分页长度选项的字符串
                loadingRecords: "加载中...",//用来描述数据在加载中等待的提示字符串 - 当异步读取数据的时候显示
                processing: "处理中...",//用来描述加载进度的字符串
                paginate: {
                  first: "首页",
                  previous: "上一页",
                  next: "下一页",
                  last: "尾页"
                  },
                aria: {
                   sortAscending: "：激活以按升序排序此列",
                   sortDescending: ": 激活以按降序排序此列"
                }
            }
        });
        // #TODO 更换为固定表格列后可不展示主键列，数据放到列数据的data-id中
        // $('#table-finance').DataTable().column(0).visible(false);
    })
}

function deleteRecord(detailId) {
    $.ajax({
        url: '/finance/' + detailId,
        type: 'DELETE',
        success: function (response) {
            if (response && response.length > 0 && response[0].Alert) {
                showWarningToast(response[0].Alert);
            } else {
                showWarningToast("数据删除成功！");
            }
        }
    });
}

function loadFinanceDetail(detailId) {
    if (typeof detailId !== 'undefined' && detailId !== null && detailId.trim() !== '') {
        $.getJSON('/finance/' + detailId, function (datas) {
        let data = datas[0];
        $('#findetail-owner').html(data.owner);
        $('#findetail-source').html(data.sourcetype);
        $('#findetail-inorout').html(data.inorout);
        $('#findetail-counterparty').html(data.counterparty);
        $('#findetail-counterbank').html(data.counterbank);
        $('#findetail-counteraccount').html(data.counteraccount);
        $('#findetail-goodscomment').html(data.goodscomment);
        $('#findetail-paymethod').html(data.paymethod);
        $('#findetail-amount').html(data.amount);
        $('#findetail-balance').html(data.balance);
        $('#findetail-currency').html(data.currency);
        $('#findetail-paystatus').html(data.paystatus);
        $('#findetail-tradetype').html(data.tradetype);
        $('#findetail-tradeorderno').html(data.tradeorderno);
        $('#findetail-counterorderno').html(data.counterorderno);
        $('#findetail-tradetime').html(data.tradetime);
        $('#findetail-billcomment').html(data.billcomment);
    });
    } else {
        showWarningToast("服务器内部错误！");
    }
}

/**
 * 读取并处理Excel/CSV文件
 * @param {File} file - 上传的文件对象
 * @param {string} importType - 导入类型
 * @param {Function} processCallback - 处理数据的回调函数
 */
function readAndProcessWorkbook(file, importType, processCallback) {
    const fileExtension = file.name.split('.').pop().toLowerCase();

    if (!['csv', 'xlsx', 'xls'].includes(fileExtension)) {
        showWarningToast(`不支持的文件格式: ${fileExtension}`);
        return;
    }

    const reader = new FileReader();
    reader.onload = function(e) {
        try {
            let workbook;
            if (fileExtension === 'csv') {
                const decoder = new TextDecoder('utf-8');
                const data = decoder.decode(new Uint8Array(e.target.result));
                workbook = XLSX.read(data, {type: 'binary'});
            } else {
                const data = new Uint8Array(e.target.result);
                workbook = XLSX.read(data, {type: 'array'});
            }

            if (!workbook.SheetNames || workbook.SheetNames.length === 0) {
                showWarningToast("Excel文件没有工作表!");
                return;
            }

            const firstSheetName = workbook.SheetNames[0];
            const worksheet = workbook.Sheets[firstSheetName];

            // 转换为JSON并清理数据
            const jsonData = XLSX.utils.sheet_to_json(worksheet, {raw: false});
            const cleanedData = jsonData.map(item => {
                const cleanedItem = {};
                Object.keys(item).forEach(key => {
                    if (item[key] !== undefined && item[key] !== null) {
                        cleanedItem[key] = String(item[key]).trim();
                    } else {
                        cleanedItem[key] = '';
                    }
                });
                return cleanedItem;
            });

            processCallback(importType, cleanedData);
        } catch (error) {
            console.error("处理文件时出错:", error);
            showWarningToast("处理文件时出错: " + error.message);
        }
    };

    reader.onerror = function() {
        showWarningToast("读取文件时出错");
    };

    reader.readAsArrayBuffer(file);
}

/**
 * 处理并上传财务数据
 * @param {string} importType - 导入类型
 * @param {Array} uploadJson - 要上传的JSON数据
 */
function processFile(importType, uploadJson) {
    if (!uploadJson || uploadJson.length === 0) {
        showWarningToast("没有可导入的数据!");
        return;
    }

    // 显示加载中提示
    showInfoToast("正在处理数据，请稍候...");

    // 根据不同的导入类型进行数据预处理
    let processedData = preprocessData(importType, uploadJson);

    const uploadParams = {
        importType: importType,
        uploadJson: processedData
    };

    // 发送AJAX请求
    $.ajax({
        url: '/upload/finance',
        type: 'POST',
        contentType: 'application/json; charset=utf-8"',
        data: JSON.stringify(uploadParams),
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                showSuccessToast(response[0].Alert);

                // 导入成功后刷新数据
                const sourceType = $('#sourceType').val();
                const inorOut = $('#inorOut').val();
                const startTime = $('#starttime').val() || defaultStartTime;
                const endTime = $('#endtime').val() || defaultEndTime;
                loadFinanceData(sourceType, inorOut, startTime, endTime);
            }
        },
        error: function(xhr, status, error) {
            console.error("上传失败:", error);
            showWarningToast("服务器内部错误！");
        }
    });
}

/**
 * 根据导入类型预处理数据
 * @param {string} importType - 导入类型
 * @param {Array} data - 原始数据
 * @returns {Array} - 处理后的数据
 */
function preprocessData(importType, data) {
    // 默认返回原始数据
    if (importType === "0") {
        return data;
    }

    // 根据不同的导入类型进行数据映射和转换
    const processedData = data.map(item => {
        const newItem = {};

        // 设置默认值
        newItem["Owner"] = $('#owner').val() || "系统导入";
        newItem["Source"] = importType;

        // 根据不同的导入类型进行字段映射
        switch (importType) {
            case "1": // 支付宝
                mapAlipayFields(item, newItem);
                break;
            case "2": // 微信
                mapWechatFields(item, newItem);
                break;
            case "3": // 青岛银行
                mapQDBankFields(item, newItem);
                break;
            case "4": // 中国银行
                mapBOCFields(item, newItem);
                break;
        }

        return newItem;
    });

    return processedData;
}

/**
 * 显示信息提示
 * @param {string} message - 提示信息
 */
function showInfoToast(message) {
    const toastElList = [].slice.call(document.querySelectorAll('.toast'));
    const toastList = toastElList.map(function (toastEl) {
        const toastBodyEl = toastEl.querySelector('.toast-body');
        toastBodyEl.textContent = message;
        return new bootstrap.Toast(toastEl);
    });
    toastList.forEach(toast => toast.show());
}

/**
 * 映射支付宝字段
 * @param {Object} source - 源数据
 * @param {Object} target - 目标数据
 */
function mapAlipayFields(source, target) {
    // 支付宝账单字段映射
    // 交易时间
    if (source["交易时间"] || source["交易创建时间"]) {
        target["TradeTime"] = parseAlipayDate(source["交易时间"] || source["交易创建时间"]);
    }

    // 交易分类
    target["TradeType"] = source["交易分类"] || source["交易类型"] || "";

    // 交易对方
    target["CounterParty"] = source["交易对方"] || source["对方"] || "";

    // 商品说明
    target["GoodsComment"] = source["商品说明"] || source["商品"] || "";

    // 收/支
    if (source["收/支"]) {
        target["InOrOut"] = source["收/支"] === "收入" ? "收入" : "支出";
    } else {
        // 根据金额判断收支
        const amount = parseFloat(source["金额"] || "0");
        target["InOrOut"] = amount >= 0 ? "收入" : "支出";
    }

    // 金额
    if (source["金额"]) {
        target["Amount"] = parseFloat(source["金额"].replace(/[¥,]/g, ""));
    }

    // 收/付款方式
    target["PayMethod"] = source["收/付款方式"] || source["支付方式"] || "";

    // 交易状态
    target["PayStatus"] = source["交易状态"] || source["状态"] || "";

    // 交易订单号
    target["TradeOrderNo"] = source["交易订单号"] || source["订单号"] || "";

    // 商家订单号
    target["CounterOrderNo"] = source["商家订单号"] || source["商户单号"] || "";

    // 备注
    target["BillComment"] = source["备注"] || "";
}

/**
 * 映射微信字段
 * @param {Object} source - 源数据
 * @param {Object} target - 目标数据
 */
function mapWechatFields(source, target) {
    // 微信账单字段映射
    // 交易时间
    if (source["交易时间"] || source["交易创建时间"]) {
        target["TradeTime"] = parseWechatDate(source["交易时间"] || source["交易创建时间"]);
    }

    // 交易订单号
    target["TradeOrderNo"] = source["交易单号"] || source["微信订单号"] || "";

    // 商家订单号
    target["CounterOrderNo"] = source["商户单号"] || source["商户订单号"] || "";

    // 交易时间
    if (source["交易时间"] || source["交易创建时间"]) {
        target["TradeTime"] = parseWechatDate(source["交易时间"] || source["交易创建时间"]);
    }

    // 收/付款方式
    target["PayMethod"] = source["支付方式"] || source["支付类型"] || "微信";

    // 交易对方
    target["CounterParty"] = source["交易对方"] || source["商户"] || "";

    // 商品说明
    target["GoodsComment"] = source["商品"] || source["商品说明"] || "";

    // 金额
    if (source["金额"] || source["收支金额"]) {
        const amountStr = source["金额"] || source["收支金额"] || "0";
        target["Amount"] = parseFloat(amountStr.replace(/[¥,]/g, ""));
    }

    // 收/支
    if (source["收/支"] || source["收支类型"]) {
        target["InOrOut"] = (source["收/支"] === "收入" || source["收支类型"] === "收入") ? "收入" : "支出";
    } else {
        // 根据金额判断收支
        const amount = parseFloat(target["Amount"] || 0);
        target["InOrOut"] = amount >= 0 ? "收入" : "支出";
    }

    // 交易状态
    target["PayStatus"] = source["当前状态"] || source["交易状态"] || "";

    // 备注
    target["BillComment"] = source["备注"] || source["交易备注"] || "";
}

/**
 * 映射青岛银行字段
 * @param {Object} source - 源数据
 * @param {Object} target - 目标数据
 */
function mapQDBankFields(source, target) {
    // 青岛银行账单字段映射
    // 交易时间
    if (source["交易日期"] || source["交易时间"]) {
        target["TradeTime"] = parseBankDate(source["交易日期"] || source["交易时间"]);
    }

    // 交易对方
    target["CounterParty"] = source["对方户名"] || source["交易对方"] || "";

    // 对方开户行
    target["CounterBank"] = source["对方开户行"] || source["对方银行"] || "";

    // 对方账号
    target["CounterAccount"] = source["对方账号"] || "";

    // 商品说明
    target["GoodsComment"] = source["摘要"] || source["交易摘要"] || "";

    // 金额
    if (source["交易金额"] || source["金额"]) {
        const amountStr = source["交易金额"] || source["金额"] || "0";
        target["Amount"] = parseFloat(amountStr.replace(/[,]/g, ""));
    }

    // 余额
    if (source["账户余额"] || source["余额"]) {
        const balanceStr = source["账户余额"] || source["余额"] || "0";
        target["Balance"] = parseFloat(balanceStr.replace(/[,]/g, ""));
    }

    // 收/支
    if (source["借贷标志"]) {
        target["InOrOut"] = source["借贷标志"] === "贷" ? "收入" : "支出";
    } else {
        // 根据金额判断收支
        const amount = parseFloat(target["Amount"] || 0);
        target["InOrOut"] = amount >= 0 ? "收入" : "支出";
    }
}

/**
 * 映射中国银行字段
 * @param {Object} source - 源数据
 * @param {Object} target - 目标数据
 */
function mapBOCFields(source, target) {
    // 中国银行账单字段映射
    // 交易时间
    if (source["交易日期"] || source["交易时间"]) {
        target["TradeTime"] = parseBankDate(source["交易日期"] || source["交易时间"]);
    }

    // 交易对方
    target["CounterParty"] = source["对方户名"] || source["交易对方"] || "";

    // 对方开户行
    target["CounterBank"] = source["对方开户行"] || source["对方银行"] || "";

    // 对方账号
    target["CounterAccount"] = source["对方账号"] || "";

    // 商品说明
    target["GoodsComment"] = source["摘要"] || source["交易摘要"] || "";

    // 金额
    if (source["交易金额"] || source["金额"]) {
        const amountStr = source["交易金额"] || source["金额"] || "0";
        target["Amount"] = parseFloat(amountStr.replace(/[,]/g, ""));
    }

    // 余额
    if (source["账户余额"] || source["余额"]) {
        const balanceStr = source["账户余额"] || source["余额"] || "0";
        target["Balance"] = parseFloat(balanceStr.replace(/[,]/g, ""));
    }

    // 收/支
    if (source["借贷标志"]) {
        target["InOrOut"] = source["借贷标志"] === "贷" ? "收入" : "支出";
    } else {
        // 根据金额判断收支
        const amount = parseFloat(target["Amount"] || 0);
        target["InOrOut"] = amount >= 0 ? "收入" : "支出";
    }
}

/**
 * 解析支付宝日期格式
 * @param {string} dateStr - 日期字符串
 * @returns {string} - 格式化后的日期
 */
function parseAlipayDate(dateStr) {
    if (!dateStr) return null;

    // 支付宝日期格式通常为: 2023-01-01 12:34:56
    try {
        const date = new Date(dateStr.replace(/-/g, '/'));
        return date.toISOString();
    } catch (e) {
        console.error("解析支付宝日期出错:", e);
        return dateStr;
    }
}

/**
 * 解析微信日期格式
 * @param {string} dateStr - 日期字符串
 * @returns {string} - 格式化后的日期
 */
function parseWechatDate(dateStr) {
    if (!dateStr) return null;

    // 微信日期格式通常为: 2023-01-01 12:34:56
    try {
        const date = new Date(dateStr.replace(/-/g, '/'));
        return date.toISOString();
    } catch (e) {
        console.error("解析微信日期出错:", e);
        return dateStr;
    }
}

/**
 * 解析银行日期格式
 * @param {string} dateStr - 日期字符串
 * @returns {string} - 格式化后的日期
 */
function parseBankDate(dateStr) {
    if (!dateStr) return null;

    // 银行日期格式可能为: 2023-01-01 或 2023/01/01 或 20230101
    try {
        let formattedDate = dateStr;

        // 处理没有分隔符的日期格式 (20230101)
        if (dateStr.length === 8 && !dateStr.includes('-') && !dateStr.includes('/')) {
            formattedDate = `${dateStr.substring(0, 4)}-${dateStr.substring(4, 6)}-${dateStr.substring(6, 8)}`;
        }

        const date = new Date(formattedDate.replace(/-/g, '/'));
        return date.toISOString();
    } catch (e) {
        console.error("解析银行日期出错:", e);
        return dateStr;
    }
}

/**
 * 从支付宝API获取交易数据
 * @param {string} startDate - 开始日期
 * @param {string} endDate - 结束日期
 */
function fetchAlipayTransactions(startDate, endDate) {
    showInfoToast("正在从支付宝获取交易数据...");

    $.ajax({
        url: '/api/finance/alipay',
        type: 'GET',
        data: {
            startDate: startDate,
            endDate: endDate
        },
        success: function(response) {
            if (response && response.success) {
                showSuccessToast("成功获取支付宝交易数据，共 " + response.count + " 条记录");

                // 刷新数据
                const sourceType = $('#sourceType').val();
                const inorOut = $('#inorOut').val();
                loadFinanceData(sourceType, inorOut, startDate, endDate);
            } else {
                showWarningToast(response.message || "获取支付宝交易数据失败");
            }
        },
        error: function(xhr, status, error) {
            console.error("获取支付宝交易数据失败:", error);
            showWarningToast("获取支付宝交易数据失败: " + error);
        }
    });
}

/**
 * 从微信支付API获取交易数据
 * @param {string} startDate - 开始日期
 * @param {string} endDate - 结束日期
 */
function fetchWechatTransactions(startDate, endDate) {
    showInfoToast("正在从微信支付获取交易数据...");

    $.ajax({
        url: '/api/finance/wechat',
        type: 'GET',
        data: {
            startDate: startDate,
            endDate: endDate
        },
        success: function(response) {
            if (response && response.success) {
                showSuccessToast("成功获取微信支付交易数据，共 " + response.count + " 条记录");

                // 刷新数据
                const sourceType = $('#sourceType').val();
                const inorOut = $('#inorOut').val();
                loadFinanceData(sourceType, inorOut, startDate, endDate);
            } else {
                showWarningToast(response.message || "获取微信支付交易数据失败");
            }
        },
        error: function(xhr, status, error) {
            console.error("获取微信支付交易数据失败:", error);
            showWarningToast("获取微信支付交易数据失败: " + error);
        }
    });
}

$(document).ready(function() {
    loadFinanceData($('#sourceType').val(), $('#inorOut').val(), defaultStartTime, defaultEndTime);

    // 查询按钮点击事件
    $('#searchFinance').click(function () {
        const sourceType = $('#sourceType').val();
        const inorOut = $('#inorOut').val();
        const startTime = $('#starttime').val();
        const endTime = $('#endtime').val();
        loadFinanceData(sourceType, inorOut, startTime, endTime);
    });

    // 清空按钮点击事件
    $('#cleanFinance').click(function () {
        $('input[type="text"]').val('');
    });

    // 刷新按钮点击事件
    $('#refresh-finance-btn').click(function() {
        const sourceType = $('#sourceType').val();
        const inorOut = $('#inorOut').val();
        const startTime = $('#starttime').val() || defaultStartTime;
        const endTime = $('#endtime').val() || defaultEndTime;
        loadFinanceData(sourceType, inorOut, startTime, endTime);
        showSuccessToast("数据已刷新");
    });

    // 导出按钮点击事件
    $('#export-finance-btn').click(function() {
        exportFinanceData();
    });

    // 导入按钮点击事件
    $('#importFinance').click(function () {
        $('#finance-import').modal('show');
    });

    // API导入按钮点击事件
    $('#apiImportFinance').click(function () {
        $('#finance-api-import').modal('show');
    });

    /**
     * 提交财务导入表单
     */
    $('#submitFinance').click(function (e) {
        e.preventDefault();
        const importType = $('#importType').val();
        const fileInput = $('#finance-imp-file')[0];
        const uploadFile = fileInput.files[0];

        // 文件验证
        if (!uploadFile) {
            showWarningToast("请选择一个文件!");
            return;
        }

        // 文件大小验证
        if (uploadFile.size > 10 * 1024 * 1024) {
            showWarningToast("文件大小超过10MB!");
            return;
        }

        // 文件类型验证
        const fileExtension = uploadFile.name.split('.').pop().toLowerCase();
        const validExtensions = ['xlsx', 'xls', 'csv'];
        if (!validExtensions.includes(fileExtension)) {
            showWarningToast("文件类型不符合要求!");
            return;
        }

        // 使用优化后的函数处理文件
        readAndProcessWorkbook(uploadFile, importType, processFile);

        // 隐藏模态框
        $('#finance-import').modal('hide');
    });

    // 导入类型变更事件
    $('#importType').on('change', function() {
        let selectedValue = $(this).val();
        let exampleLink = $('#exampleLink');

        switch (selectedValue) {
            case '0':
                exampleLink.attr('href', '/assets/files/finance-import-sample-raw.xlsx');
                break;
            case '1':
                exampleLink.attr('href', '/assets/files/finance-import-sample-alipay.xlsx');
                break;
            case '2':
                exampleLink.attr('href', '/assets/files/finance-import-sample-weixin.xlsx');
                break;
            case '3':
                exampleLink.attr('href', '/assets/files/finance-import-sample-bqd.xlsx');
                break;
            case '4':
                exampleLink.attr('href', '/assets/files/finance-import-sample-boc.xlsx');
                break;
            default:
                exampleLink.attr('href', '/assets/files/finance-import-sample-raw.xlsx');
                break;
        }
    });

    // API导入类型变更事件
    $('#apiImportType').on('change', function() {
        const selectedValue = $(this).val();

        // 根据选择的API类型显示或隐藏相应的配置选项
        if (selectedValue === '1') {
            $('#alipayConfigSection').show();
            $('#wechatConfigSection').hide();
        } else if (selectedValue === '2') {
            $('#alipayConfigSection').hide();
            $('#wechatConfigSection').show();
        }
    });

    // 提交API导入表单
    $('#submitApiImport').click(function(e) {
        e.preventDefault();

        const apiImportType = $('#apiImportType').val();
        const startDate = $('#apiStartDate').val();
        const endDate = $('#apiEndDate').val();

        if (!startDate || !endDate) {
            showWarningToast("请选择开始和结束日期");
            return;
        }

        // 根据选择的API类型调用相应的函数
        if (apiImportType === '1') {
            fetchAlipayTransactions(startDate, endDate);
        } else if (apiImportType === '2') {
            fetchWechatTransactions(startDate, endDate);
        }

        // 隐藏模态框
        $('#finance-api-import').modal('hide');
    });

    // 保存API配置
    $('#saveApiConfig').click(function(e) {
        e.preventDefault();

        const apiImportType = $('#apiImportType').val();
        let configData = {};

        if (apiImportType === '1') {
            // 支付宝配置
            configData = {
                type: 'alipay',
                appId: $('#alipayAppId').val(),
                privateKey: $('#alipayPrivateKey').val(),
                publicKey: $('#alipayPublicKey').val()
            };
        } else if (apiImportType === '2') {
            // 微信支付配置
            configData = {
                type: 'wechat',
                appId: $('#wechatAppId').val(),
                mchId: $('#wechatMchId').val(),
                apiKey: $('#wechatApiKey').val(),
                apiV3Key: $('#wechatApiV3Key').val(),
                serialNo: $('#wechatSerialNo').val(),
                privateKey: $('#wechatPrivateKey').val()
            };
        }

        // 发送配置到服务器
        $.ajax({
            url: '/api/finance/config',
            type: 'POST',
            contentType: 'application/json; charset=utf-8',
            data: JSON.stringify(configData),
            success: function(response) {
                if (response && response.success) {
                    showSuccessToast("API配置保存成功");
                } else {
                    showWarningToast(response.message || "API配置保存失败");
                }
            },
            error: function(xhr, status, error) {
                console.error("API配置保存失败:", error);
                showWarningToast("API配置保存失败: " + error);
            }
        });
    });

    let dataTableFinance = $('#table-finance').DataTable();
    let dataTableFinanceBody = $('body');

    dataTableFinanceBody.on('dblclick', '#table-finance tbody tr', function() {
        const detailId = $(this).find('td').eq(0).text();
        if (detailId  !== "未查到数据") {
            loadFinanceDetail(detailId);
            $('#finance-detail').modal('show');
        }
    });

    dataTableFinance.on('click', '.delete-btn', function() {
        let delRow = $(this).closest('tr');
        $('#del-confirm').modal('show');
        $('#del-confirm-btn').click(function () {
            let idCell = delRow.find('td').first();
            let detailId = idCell.text();
            if (detailId !== "未查到数据" && typeof detailId !== 'undefined' && detailId !== null && detailId.trim() !== '') {
                deleteRecord(detailId);
                delRow.remove();
                setTimeout(function () {
                    dataTableFinance.draw(false);
                }, 100);
            } else {
                showWarningToast("未查到需删除数据，请刷新页面重试!");
            }
        });
    });
});
