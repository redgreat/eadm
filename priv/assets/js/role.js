/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-26 15:55:59
 *
 * Module : role.js
 *
 */

function translateColumnNames(columnName) {
  const translations = i18nRole.columnName[defaultLanguage];
  return translations[columnName] || columnName;
}


function loadRoleData() {
    let dynamicColumns = []
    let dynamicDatas = []
    $.getJSON('/data/role', function (resdata) {
        function buildDynamicData(resdata) {
            resdata.columns.forEach(function (column) {
                let dynamicColumn = {};
                dynamicColumn['data'] = column;
                dynamicColumn['title'] = translateColumnNames(column);
                dynamicColumns.push(dynamicColumn);
            });
            dynamicColumns.push({"data": "Action", "title": "操作"});
            dynamicDatas = resdata.data;
        }

        if (resdata && resdata.length > 0 && resdata[0].Alert) {
            showWarningToast(resdata[0].Alert);
        } else {
            buildDynamicData(resdata);
        }

        $('#table-role').DataTable().destroy();
        $('#table-role').empty();
        $('#table-role').DataTable({
            // lengthChange: true,  //是否允许用户改变表格每页显示的记录数
            // bStateSave: true,  //记录cookie
            columnDefs: [{
                targets: -1, // 将按钮添加到最后一列
                render: function () {
                    return `
                        <button class="btn btn-outline-primary btn-rounded disable-role-btn">
                            <i class="fas fa-toggle-on"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded edit-role-btn">
                            <i class="fas fa-pen"></i>
                        </button>
                    `;
                }
            },{
                targets: 0,
                visible: false,
                orderable: false
            }],
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
    })
}

function disableRole(roleId) {
    if (typeof roleId !== 'undefined' && roleId !== null && roleId.trim() !== '') {
        $.ajax({
            url: '/data/role/disable/' + roleId,
            type: 'POST',
            success: function (resdata) {
                if (resdata && resdata.length > 0 && resdata[0].Alert) {
                    showWarningToast(resdata[0].Alert);
                } else {
                    showWarningToast("服务器运行错误，请联系管理员！");
                }
            }
        });
    }
}

function editPermission(roleId) {
    if (typeof roleId !== 'undefined' && roleId !== null && roleId.trim() !== '') {
        const postParams = {
            roleId: roleId,
            dashBoard: $('#dashboard').is(':checked'),
            health: $('#health').is(':checked'),
            locate: $('#locate').is(':checked'),
            finance: $('#finance').is(':checked'),
            finimp: $('#finance-imp').is(':checked'),
            findel: $('#finance-del').is(':checked'),
            crontab: $('#crontab').is(':checked'),
            userManage: $('#usermanage').is(':checked')
        }
        $.ajax({
            url: '/data/permission',
            type: 'POST',
            data: postParams,
            success: function (resdata) {
                if (resdata && resdata.length > 0 && resdata[0].Alert) {
                    showWarningToast(resdata[0].Alert);
                } else {
                    showWarningToast("服务器运行错误，请联系管理员！");
                }
            }
        });
    }
}

function loadPermission(roleId) {
    if (typeof roleId !== 'undefined' && roleId !== null && roleId.trim() !== '') {
        $.getJSON('/data/permission/' + roleId, function (resdata) {
            $('#dashboard').prop('checked', resdata.dashboard);
            $('#health').prop('checked', resdata.health);
            $('#locate').prop('checked', resdata.locate);
            $('#finance').prop('checked', resdata.finance.finlist);
            $('#finance-imp').prop('checked', resdata.finance.finimp);
            $('#finance-del').prop('checked', resdata.finance.findel);
            $('#crontab').prop('checked', resdata.crontab);
            $('#usermanage').prop('checked', resdata.usermanage);
        });
    }
}

$(document).ready(function() {

    loadRoleData();

    $('#refresh-role-btn').click(function () {
        loadRoleData();
        showWarningToast("数据刷新成功！");
    });

    $('#edit-role-submit-btn').click(function (e) {
        let roleId = $('#role-edit .modal-header .modal-title').data('id');
        editPermission(roleId);
        showWarningToast("角色信息变更成功！");
    });

    let dataTableRole = $('#table-role').DataTable();

    dataTableRole.on('click', '.disable-role-btn', function() {
        let disableRow = $(this).closest('tr');
        let idCell = disableRow.find('td').first();
        let roleId = idCell.text();
        if (roleId !== "未查到数据" && typeof roleId !== 'undefined' && roleId !== null && roleId.trim() !== '') {
            disableRole(roleId);
            setTimeout(function () {
                loadRoleData();
            }, 100);
        } else {
            showWarningToast("未查到需禁用角色，请刷新页面重试!");
        }
    });

    dataTableRole.on('click', '.edit-role-btn', function() {
        let editRow = $(this).closest('tr');
        let idCell = editRow.find('td').first();
        let roleId = idCell.text();
        if (roleId !== "未查到数据" && typeof roleId !== 'undefined' && roleId !== null && roleId.trim() !== '') {
            $('#role-edit').modal('show');
            loadPermission(roleId);
            let modalTitle = $('.modal-title');
            modalTitle.attr('data-id', roleId);
        } else {
            showWarningToast("未查到需编辑角色，请刷新页面重试!");
        }
    });

});
