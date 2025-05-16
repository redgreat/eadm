/**
 * 设备管理页面JavaScript
 * @author wangcw
 * @date 2024-07-01
 */

// 全局变量
let deviceTable;
let currentDeviceNo = '';
let delType = ''; // 'device' 或 'user'
let delId = '';

$(document).ready(function() {
    // 初始化设备表格
    initDeviceTable();

    // 查询按钮点击事件
    $('#searchDevice').click(function() {
        loadDeviceData();
    });

    // 清空按钮点击事件
    $('#cleanDevice').click(function() {
        $('#deviceNo').val('');
    });

    // 刷新按钮点击事件
    $('#refresh-device-btn').click(function() {
        loadDeviceData();
    });

    // 新增设备提交事件
    $('#submit-device-add').click(function() {
        addDevice();
    });

    // 编辑设备提交事件
    $('#submit-device-edit').click(function() {
        editDevice();
    });

    // 添加设备用户提交事件
    $('#submit-add-device-user').click(function() {
        addDeviceUser();
    });

    // 删除确认按钮点击事件
    $('#del-confirm-btn').click(function() {
        if (delType === 'device') {
            deleteDevice(delId);
        } else if (delType === 'user') {
            unassignDeviceUser(delId);
        }
        $('#del-confirm').modal('hide');
    });

    // 加载初始数据
    loadDeviceData();

    // 加载用户列表
    loadUserList();
});

/**
 * 初始化设备表格
 */
function initDeviceTable() {
    deviceTable = $('#table-device').DataTable({
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
        },
        columns: [
            { data: 'deviceno', title: '设备号' },
            { data: 'imei', title: 'IMEI' },
            { data: 'simno', title: 'SIM卡号' },
            { data: 'remark', title: '备注' },
            {
                data: 'enable',
                title: '状态',
                render: function(data) {
                    return data ? '<span class="badge bg-success">启用</span>' : '<span class="badge bg-danger">禁用</span>';
                }
            },
            {
                data: 'createdat',
                title: '创建时间',
                render: function(data) {
                    return formatDateTime(data);
                }
            },
            {
                data: null,
                title: '操作',
                render: function(data) {
                    return '<div class="btn-group" role="group">' +
                        '<button type="button" class="btn btn-sm btn-outline-primary edit-device" data-deviceno="' + data.deviceno + '">' +
                        '<i class="fas fa-edit"></i></button>' +
                        '<button type="button" class="btn btn-sm btn-outline-danger delete-device" data-deviceno="' + data.deviceno + '">' +
                        '<i class="fas fa-trash-alt"></i></button>' +
                        '<button type="button" class="btn btn-sm btn-outline-info manage-users" data-deviceno="' + data.deviceno + '">' +
                        '<i class="fas fa-users"></i></button>' +
                        '</div>';
                }
            }
        ],
        order: [[5, 'desc']]
    });

    // 编辑设备按钮点击事件
    $('#table-device').on('click', '.edit-device', function() {
        const deviceNo = $(this).data('deviceno');
        loadDeviceDetail(deviceNo);
    });

    // 删除设备按钮点击事件
    $('#table-device').on('click', '.delete-device', function() {
        const deviceNo = $(this).data('deviceno');
        delType = 'device';
        delId = deviceNo;
        $('#del-confirm').modal('show');
    });

    // 管理用户按钮点击事件
    $('#table-device').on('click', '.manage-users', function() {
        const deviceNo = $(this).data('deviceno');
        currentDeviceNo = deviceNo;
        $('#device-users-deviceNo').text(deviceNo);
        loadDeviceUsers(deviceNo);
        $('#modal-device-users').modal('show');
    });
}

/**
 * 加载设备数据
 */
function loadDeviceData() {
    const deviceNo = $('#deviceNo').val() || '';

    $.ajax({
        url: '/device',
        type: 'GET',
        data: {
            deviceNo: deviceNo
        },
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                showWarningToast(response[0].Alert);
            } else {
                deviceTable.clear().rows.add(response).draw();
            }
        },
        error: function() {
            showWarningToast('加载设备数据失败！');
        }
    });
}

/**
 * 加载设备详情
 * @param {string} deviceNo - 设备号
 */
function loadDeviceDetail(deviceNo) {
    const devices = deviceTable.data().toArray();
    const device = devices.find(d => d.deviceno === deviceNo);

    if (device) {
        $('#edit-deviceNo').val(device.deviceno);
        $('#edit-imei').val(device.imei);
        $('#edit-simNo').val(device.simno);
        $('#edit-remark').val(device.remark);
        $('#edit-enable').prop('checked', device.enable);

        $('#modal-device-edit').modal('show');
    }
}

/**
 * 添加设备
 */
function addDevice() {
    const deviceNo = $('#add-deviceNo').val();
    const imei = $('#add-imei').val();
    const simNo = $('#add-simNo').val();
    const remark = $('#add-remark').val();

    if (!deviceNo) {
        showWarningToast('设备号不能为空！');
        return;
    }

    $.ajax({
        url: '/device/add',
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify({
            deviceNo: deviceNo,
            imei: imei || '',
            simNo: simNo || '',
            remark: remark || ''
        }),
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                if (response[0].Alert === '设备添加成功！') {
                    showSuccessToast(response[0].Alert);
                    $('#modal-device-add').modal('hide');
                    $('#add-deviceNo').val('');
                    $('#add-imei').val('');
                    $('#add-simNo').val('');
                    $('#add-remark').val('');
                    loadDeviceData();
                } else {
                    showWarningToast(response[0].Alert);
                }
            }
        },
        error: function() {
            showWarningToast('添加设备失败！');
        }
    });
}

/**
 * 编辑设备
 */
function editDevice() {
    const deviceNo = $('#edit-deviceNo').val();
    const imei = $('#edit-imei').val();
    const simNo = $('#edit-simNo').val();
    const remark = $('#edit-remark').val();
    const enable = $('#edit-enable').is(':checked');

    $.ajax({
        url: '/device/edit',
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify({
            deviceNo: deviceNo,
            imei: imei || '',
            simNo: simNo || '',
            remark: remark || '',
            enable: enable
        }),
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                if (response[0].Alert === '设备更新成功！') {
                    showSuccessToast(response[0].Alert);
                    $('#modal-device-edit').modal('hide');
                    loadDeviceData();
                } else {
                    showWarningToast(response[0].Alert);
                }
            }
        },
        error: function() {
            showWarningToast('更新设备失败！');
        }
    });
}

/**
 * 删除设备
 * @param {string} deviceNo - 设备号
 */
function deleteDevice(deviceNo) {
    $.ajax({
        url: '/device/delete/' + deviceNo,
        type: 'DELETE',
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                if (response[0].Alert === '设备删除成功！') {
                    showSuccessToast(response[0].Alert);
                    loadDeviceData();
                } else {
                    showWarningToast(response[0].Alert);
                }
            }
        },
        error: function() {
            showWarningToast('删除设备失败！');
        }
    });
}

/**
 * 加载设备用户列表
 * @param {string} deviceNo - 设备号
 */
function loadDeviceUsers(deviceNo) {
    $.ajax({
        url: '/device/users/' + deviceNo,
        type: 'GET',
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                showWarningToast(response[0].Alert);
            } else {
                let html = '';
                response.forEach(function(user) {
                    html += '<tr>' +
                        '<td>' + user.id + '</td>' +
                        '<td>' + user.userid + '</td>' +
                        '<td>' + user.loginname + '</td>' +
                        '<td>' + user.username + '</td>' +
                        '<td>' +
                        '<button type="button" class="btn btn-sm btn-outline-danger unassign-user" data-id="' + user.id + '">' +
                        '<i class="fas fa-user-minus"></i></button>' +
                        '</td>' +
                        '</tr>';
                });
                $('#device-users-body').html(html);
            }
        },
        error: function() {
            showWarningToast('加载设备用户失败！');
        }
    });
}

/**
 * 加载用户列表
 */
function loadUserList() {
    $.ajax({
        url: '/user',
        type: 'GET',
        data: {
            loginName: ''
        },
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                showWarningToast(response[0].Alert);
            } else {
                let html = '<option value="" selected disabled>请选择用户</option>';
                response.forEach(function(user) {
                    html += '<option value="' + user.id + '" data-loginname="' + user.loginname + '">' +
                        user.username + ' (' + user.loginname + ')</option>';
                });
                $('#user-select').html(html);
            }
        },
        error: function() {
            showWarningToast('加载用户列表失败！');
        }
    });
}

/**
 * 添加设备用户
 */
function addDeviceUser() {
    const userId = $('#user-select').val();
    const userLoginName = $('#user-select option:selected').data('loginname');

    if (!userId) {
        showWarningToast('请选择用户！');
        return;
    }

    $.ajax({
        url: '/device/assign',
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify({
            deviceNo: currentDeviceNo,
            userId: userId,
            userLoginName: userLoginName
        }),
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                if (response[0].Alert === '设备分配成功！') {
                    showSuccessToast(response[0].Alert);
                    $('#modal-add-device-user').modal('hide');
                    loadDeviceUsers(currentDeviceNo);
                } else {
                    showWarningToast(response[0].Alert);
                }
            }
        },
        error: function() {
            showWarningToast('分配设备失败！');
        }
    });
}

/**
 * 取消设备用户分配
 * @param {string} id - 分配记录ID
 */
function unassignDeviceUser(id) {
    $.ajax({
        url: '/device/unassign/' + id,
        type: 'DELETE',
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                if (response[0].Alert === '设备取消分配成功！') {
                    showSuccessToast(response[0].Alert);
                    loadDeviceUsers(currentDeviceNo);
                } else {
                    showWarningToast(response[0].Alert);
                }
            }
        },
        error: function() {
            showWarningToast('取消分配设备失败！');
        }
    });
}

// 设备用户表格中的取消分配按钮点击事件
$(document).on('click', '.unassign-user', function() {
    const id = $(this).data('id');
    delType = 'user';
    delId = id;
    $('#del-confirm').modal('show');
});

// 这些函数已经在utils.js中定义，这里不再重复定义
