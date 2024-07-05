/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-20 16:30:54
 *
 * Module : user.js
 *
 */

function translateColumnNames(columnName) {
  const translations = i18nUser.columnName[defaultLanguage];
  return translations[columnName] || columnName;
}

function loadUserData() {
    let dynamicColumns = []
    let dynamicDatas = []
    $.getJSON('/data/user', function (resdata) {
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

        $('#table-user').DataTable().destroy();
        $('#table-user').empty();
        $('#table-user').DataTable({
            // lengthChange: true,  //是否允许用户改变表格每页显示的记录数
            // bStateSave: true,  //记录cookie
            columnDefs: [{
                targets: -1, // 将按钮添加到最后一列
                render: function (data, type, full, meta) {
                    return `
                        <button class="btn btn-outline-primary btn-rounded user-role-btn"
                          data-bs-toggle="modal" data-bs-target="#modal-user-role"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="编辑用户角色">
                          <i class="fas fa-user-shield"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded edit-user-btn"
                          data-bs-toggle="modal" data-bs-target="#modal-user-edit"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="编辑用户信息">
                          <i class="fas fa-pen"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded disable-user-btn"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="启禁用户">
                          <i class="fas fa-toggle-on"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded reset-user-btn"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="重置密码">
                          <i class="fas fa-user-cog"></i>
                        </button>
                        <button class="btn btn-outline-danger btn-rounded delete-user-btn"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="删除用户">
                          <i class="fas fa-trash"></i>
                        </button>
                    `;
                }
            },{
                targets: 0,
                visible: false,
                orderable: false
            }],
            createdRow: function(row, data) {
                let dataId = data['id'];
                $(row).attr('data-id', dataId);
            },
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

function deleteUser(userId) {
    if (typeof userId !== 'undefined' && userId !== null) {
        $.ajax({
            url: '/data/user/' + userId,
            type: 'DELETE',
            success: function (resdata) {
                if (resdata && resdata.length > 0 && resdata[0].Alert) {
                    showWarningToast(resdata[0].Alert);
                } else {
                    showWarningToast("数据删除成功！");
                }
            }
        });
    }
}

function disableUser(userId) {
    if (typeof userId !== 'undefined' && userId !== null) {
        $.ajax({
            url: '/data/user/disable/' + userId,
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

function resetUser(userId) {
    if (typeof userId !== 'undefined' && userId !== null) {
        $.ajax({
            url: '/data/user/reset/' + userId,
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

function addUser() {
    const AddParams = {
        loginName: $('#loginname').val(),
        email: $('#email').val(),
        userName: $('#username').val(),
        password: $('#password').val()
    };
    $.ajaxSetup({async:false});
    $.ajax({
        url: '/data/useradd',
        type: 'POST',
        data: AddParams,
        success: function (resdata) {
            if (resdata && resdata.length > 0 && resdata[0].Alert) {
                showWarningToast(resdata[0].Alert);
            } else {
                    showWarningToast("服务器运行错误，请联系管理员！");
            }
        }
    });
}

function loadUserRole(userId) {
    let dynamicColumns = []
    let dynamicDatas = []
    $.getJSON('/data/userrole/' + userId, function (resdata) {
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

        $('#table-user-role').DataTable().destroy();
        $('#table-user-role').empty();
        $('#table-user-role').DataTable({
            columnDefs: [{
                targets: -1,
                render: function () {
                    return `
                        <button class="btn btn-outline-danger btn-rounded delete-userrole-btn">
                          <i class="fas fa-trash"></i>
                        </button>
                    `;
                }
            },{
                targets: 0,
                visible: false,
                orderable: false
            }],
            createdRow: function(row, data) {
                let dataId = data['id'];
                $(row).attr('data-id', dataId);
            },
            destroy: true,
            columns: dynamicColumns,
            data: dynamicDatas,
            responsive: true,
            info: true,
            processing: true,
            orderMulti: true,
            ordering: true,
            paging: true,
            pageLength: 10,
            lengthChange: false,
            pagingType: "full_numbers",
            searching: false,
            stateSave: true,
            deferRender: true,
            language: {
                info: "当前 _START_ 条到 _END_ 条 共 _TOTAL_ 条",
                infoEmpty: "无记录",
                emptyTable: "未查到数据",
                thousands: ",",
                lengthMenu: "每页 _MENU_ 条记录",
                loadingRecords: "加载中...",
                processing: "处理中...",
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

function deleteUserRole(userRoleId) {
    $.ajax({
        url: '/data/userrole/delete/' + userRoleId,
        type: 'DELETE',
        success: function (resdata) {
            if (resdata && resdata.length > 0 && resdata[0].Alert) {
                showWarningToast(resdata[0].Alert);
            } else {
                showWarningToast("数据删除成功！");
            }
        }
    });
}

function loadRoleList(userId) {
    let dynamicColumns = []
    let dynamicDatas = []
    // $.ajaxSetup({async:false});
    $.getJSON('/data/rolelist/' + userId, function (resdata) {
        function buildDynamicData(resdata) {
            dynamicColumns.push({"data": "Action", "title": "选择"});
            resdata.columns.forEach(function (column) {
                let dynamicColumn = {};
                dynamicColumn['data'] = column;
                dynamicColumn['title'] = translateColumnNames(column);
                dynamicColumns.push(dynamicColumn);
            });
            dynamicDatas = resdata.data;
        }

        if (resdata && resdata.length > 0 && resdata[0].Alert) {
            showWarningToast(resdata[0].Alert);
        } else {
            buildDynamicData(resdata);
        }

        $('#table-rolelist').DataTable().destroy();
        $('#table-rolelist').empty();
        $('#table-rolelist').DataTable({
            columnDefs: [{
                targets: 0,
                render: function () {
                    return `
                        <input type="checkbox" class="select-row">
                    `;
                }
            },{
                targets: 1,
                visible: false,
                orderable: false
            }],
            createdRow: function(row, data) {
                let dataId = data['id'];
                $(row).attr('data-id', dataId);
            },
            select: {
                style: 'multi',
                selector: 'td:first-child input[type="checkbox"]'
            },
            destroy: true,
            columns: dynamicColumns,
            data: dynamicDatas,
            responsive: true,
            info: true,
            processing: true,
            orderMulti: true,
            ordering: true,
            paging: true,
            pageLength: 10,
            lengthChange: false,
            pagingType: "full_numbers",
            searching: false,
            stateSave: true,
            deferRender: true,
            language: {
                info: "当前 _START_ 条到 _END_ 条 共 _TOTAL_ 条",
                infoEmpty: "无记录",
                emptyTable: "未查到数据",
                thousands: ",",
                lengthMenu: "每页 _MENU_ 条记录",
                loadingRecords: "加载中...",
                processing: "处理中...",
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

function addUserRole(userRoleIds) {
    $.ajaxSetup({async:false});
    $.ajax({
        url: '/data/userroleadd',
        type: 'POST',
        data: userRoleIds,
        success: function (resdata) {
            if (resdata && resdata.length > 0 && resdata[0].Alert) {
                showWarningToast(resdata[0].Alert);
            } else {
                showWarningToast("服务器运行错误，请联系管理员！");
            }
        }
    });
}

function editUser(currentUserId) {
    const AddParams = {
        userId: currentUserId,
        loginName: $('#loginname-edit').val(),
        email: $('#email-edit').val(),
        userName: $('#username-edit').val()
    };
    $.ajaxSetup({async:false});
    $.ajax({
        url: '/data/useredit',
        type: 'POST',
        data: AddParams,
        success: function (resdata) {
            if (resdata && resdata.length > 0 && resdata[0].Alert) {
                showWarningToast(resdata[0].Alert);
            } else {
                showWarningToast("服务器运行错误，请联系管理员！");
            }
        }
    });
}

$(document).ready(function() {
    let currentUserId;

    loadUserData();

    $('#refresh-user-btn').click(function () {
        loadUserData();
        showWarningToast("数据刷新成功！");
    });

    $('#add-user-submit-btn').click(function () {
        addUser();
        loadUserData();
        $('#loginname').val('');
        $('#email').val('');
        $('#username').val('');
        $('#password').val('');
    });

    $('#add-user-cancel-btn').click(function () {
        $('#loginname').val('');
        $('#email').val('');
        $('#username').val('');
        $('#password').val('');
    });

    $('#add-user-role-btn').click(function() {
        loadRoleList(currentUserId);
    });

    $('#user-role-submit-btn').click(function() {
        let userRoleIds = [];
        $('#table-rolelist tbody').find('input.select-row:checked').each(function () {
            let row = $(this).closest('tr');
            let roleId = row.data('id');
            if (roleId) {
                const userRoleId = {
                    userId: currentUserId,
                    roleId: roleId
                }
                userRoleIds.push(userRoleId);
            }
        });
        addUserRole(JSON.stringify(userRoleIds));
    });

    $('#user-edit-submit-btn').click(function() {
        editUser(currentUserId);
        loadUserData();
        $('#loginname-edit').val('');
        $('#email-edit').val('');
        $('#username-edit').val('');
    });

    let dataTableUser = $('#table-user').DataTable();

    dataTableUser.on('click', '.delete-user-btn', function() {
        let delRow = $(this).closest('tr');
        $('#del-user-confirm').modal('show');
        $('#del-user-confirm-btn').click(function () {
            let userId = delRow.data('id');
            if (userId !== "未查到数据" && typeof userId !== 'undefined' && userId !== null) {
                deleteUser(userId);
                delRow.remove();
                setTimeout(function () {
                    dataTableUser.draw(false);
                }, 100);
            } else {
                showWarningToast("未查到需删除数据，请刷新页面重试!");
            }
        });
    });

    dataTableUser.on('click', '.disable-user-btn', function() {
        let disableRow = $(this).closest('tr');
        let userId = disableRow.data('id');
        if (userId !== "未查到数据" && typeof userId !== 'undefined' && userId !== null) {
            disableUser(userId);
            setTimeout(function () {
                loadUserData();
            }, 100);
        } else {
            showWarningToast("未查到需禁用用户，请刷新页面重试!");
        }
    });

    dataTableUser.on('click', '.reset-user-btn', function() {
        let resetRow = $(this).closest('tr');
        let userId = resetRow.data('id');
        if (userId !== "未查到数据" && typeof userId !== 'undefined' && userId !== null) {
            resetUser(userId);
            setTimeout(function () {
                loadUserData();
            }, 100);
        } else {
            showWarningToast("未查到需重置用户，请刷新页面重试!");
        }

    });

    dataTableUser.on('click', '.user-role-btn', function() {
        let selectedRow = $(this).closest('tr');
        let userId = selectedRow.data('id');
        if (userId !== "未查到数据" && typeof userId !== 'undefined' && userId !== null) {
            loadUserRole(userId);
            currentUserId = userId;
        } else {
            showWarningToast("未找到用户信息，请刷新页面重试!");
        }
    });

    dataTableUser.on('click', '.edit-user-btn', function() {
        let editRow = $(this).closest('tr');
        currentUserId = editRow.data('id');
        $('#loginname-edit').val(editRow.find('td')[1].innerText);
        $('#username-edit').val(editRow.find('td')[2].innerText);
        $('#email-edit').val(editRow.find('td')[3].innerText);
    });

    let dataTableUserRole = $('#table-user-role').DataTable();

    dataTableUserRole.on('click', '.delete-userrole-btn', function() {
        let delRow = $(this).closest('tr');
        $('#del-userrole-confirm').modal('show');
        $('#del-userrole-confirm-btn').click(function () {
            let userRoleId = delRow.data('id');
            console.log("userRoleId: " + userRoleId);
            if (userRoleId !== "未查到数据" && typeof userRoleId !== 'undefined' && userRoleId !== null) {
                deleteUserRole(userRoleId);
                delRow.remove();
                setTimeout(function () {
                    dataTableUserRole.draw(false);
                }, 100);
            } else {
                showWarningToast("未查到需删除数据，请刷新页面重试!");
            }
        });
    });
});
