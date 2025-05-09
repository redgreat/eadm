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

function readAndProcessWorkbook(file, importType, processCallback) {
    const fileExtension = file.name.split('.').pop().toLowerCase();
    const reader = new FileReader();
    reader.onload = function(e) {
        let data;
        if (fileExtension === 'csv') {
            const decoder = new TextDecoder('utf-8');
            data = decoder.decode(new Uint8Array(e.target.result));
            let workbook = XLSX.read(data, {type: 'binary'});
        } else if (['xlsx', 'xls'].includes(fileExtension)) {
            data = new Uint8Array(e.target.result);
            let workbook = XLSX.read(data, {type: 'array'});
        } else {
            throw new Error(`Unsupported file extension: ${fileExtension}`);
        }
        const firstSheetName = workbook.SheetNames[0];
        if (!firstSheetName) {
            throw new Error("Excel file has no worksheets");
        }
        const worksheet = workbook.Sheets[firstSheetName];
        const jsonData = XLSX.utils.sheet_to_json(worksheet, {raw: false});
        const cleanedData = jsonData.map(item => {
            Object.keys(item).forEach(key => {
                if (item[key] !== undefined && item[key] !== null) {
                    item[key] = String(item[key]).trim();
                }
            });
            return item;
        });
        processCallback(importType, cleanedData);
    };
    reader.readAsArrayBuffer(file);
}

function processFile(importType, uploadJson) {

    const uploadParams = {
        importType: importType,
        uploadJson: uploadJson
    };
    // console.log("uploadParams: "+ JSON.stringify(uploadParams));

    // 发送AJAX请求
    $.ajax({
        url: '/upload/finance',
        type: 'POST',
        contentType: 'application/json; charset=utf-8"',
        data: JSON.stringify(uploadParams),
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                showWarningToast(response[0].Alert);
            }
        },
        error: function(xhr, status, error) {
            showWarningToast("服务器内部错误！");
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

    $('#importFinance').click(function () {
        $('#finance-import').modal('show');
    });

    $('#submitFinance').click(function (e) {
        e.preventDefault();
        const importType = $('#importType').val();
        let fileInput = $('#finance-imp-file')[0];
        let uploadFile = fileInput.files[0];
        let workbook;

        if (uploadFile) {
            if (uploadFile.size <= 10 * 1024 * 1024) {
                let fileExtension = uploadFile.name.split('.').pop().toLowerCase();
                let validExtensions = ['xlsx', 'xls', 'csv'];

                if ($.inArray(fileExtension, validExtensions) !== -1) {

                    let reader = new FileReader();
                    reader.onload = function(e) {
                        if (fileExtension === 'csv') {
                            const decoder = new TextDecoder('utf-8');
                            let data = decoder.decode(new Uint8Array(e.target.result));
                            let workbook = XLSX.read(data, {type: 'binary'});
                            let firstSheetName = workbook.SheetNames[0];
                            if (firstSheetName) {
                                let worksheet = workbook.Sheets[firstSheetName];
                                let jsonData = XLSX.utils.sheet_to_json(worksheet, {raw: false});
                                let cleanedData = jsonData.map(item => {
                                    Object.keys(item).forEach(key => {
                                        if (item[key] !== undefined && item[key] !== null) {
                                            item[key] = String(item[key]).trim();
                                        }
                                    });
                                    return item;
                                });
                                processFile(importType, cleanedData);

                            } else {
                                const toastElList = [].slice.call(document.querySelectorAll('.toast'));
                                const toastList = toastElList.map(function (toastEl) {
                                    const toastBodyEl = toastEl.querySelector('.toast-body');
                                    toastBodyEl.textContent = "Excel文件没有工作表!";
                                    return new bootstrap.Toast(toastEl);
                                });
                                toastList.forEach(toast => toast.show());
                            }
                        } else {
                            let data = new Uint8Array(e.target.result);
                            let workbook = XLSX.read(data, {type: 'array'});
                            let firstSheetName = workbook.SheetNames[0];
                            if (firstSheetName) {
                                let worksheet = workbook.Sheets[firstSheetName];
                                let jsonData = XLSX.utils.sheet_to_json(worksheet, {raw: false});
                                let cleanedData = jsonData.map(item => {
                                    Object.keys(item).forEach(key => {
                                        if (item[key] !== undefined && item[key] !== null) {
                                            item[key] = String(item[key]).trim();
                                        }
                                    });
                                    return item;
                                });
                                processFile(importType, cleanedData);

                            } else {
                                const toastElList = [].slice.call(document.querySelectorAll('.toast'));
                                const toastList = toastElList.map(function (toastEl) {
                                    const toastBodyEl = toastEl.querySelector('.toast-body');
                                    toastBodyEl.textContent = "Excel文件没有工作表!";
                                    return new bootstrap.Toast(toastEl);
                                });
                                toastList.forEach(toast => toast.show());
                            }
                        }

                    };
                    reader.readAsArrayBuffer(uploadFile);
                } else {
                    showWarningToast("文件类型不符合要求!");
                }
            } else {
                showWarningToast("文件大小超过10MB!");
            }
        } else {
            showWarningToast("请选择一个文件!");
        }
        $('#finance-import').modal('hide');
    });

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
