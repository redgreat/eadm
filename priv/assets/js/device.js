/**
 * 设备管理页面JavaScript
 * @author wangcw
 * @date 2024-07-01
 */

// 全局变量
let deviceTable;
let currentDeviceNo = ''; // 当前设备号
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
        // 表格基本配置
        responsive: true,
        info: true, // 显示左下角分页信息
        processing: true, // 显示处理状态
        orderMulti: true, // 启用多列排序
        ordering: true, // 使用排序
        paging: true, // 是否分页
        pageLength: 10, // 每页默认行数
        lengthChange: false, // 禁止改变每页显示的记录数
        pagingType: "full_numbers", // 分页按钮类型
        searching: false, // 禁用本地搜索
        stateSave: true, // 刷新时保存状态
        deferRender: true, // 延迟渲染

        // 语言配置
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

        // 将设备号存储在行元素的data-deviceno属性中
        createdRow: function(row, data) {
            $(row).attr('data-deviceno', data['deviceno']);
        },

        // 列定义
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
                className: 'action-column', // 添加自定义类名，用于CSS样式
                width: '220px', // 设置列宽
                render: function(data) {
                    return `
                        <button class="btn btn-outline-primary btn-rounded edit-device"
                          data-deviceno="${data.deviceno}"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="编辑设备">
                          <i class="fas fa-pen"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded toggle-device"
                          data-deviceno="${data.deviceno}"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="启禁设备">
                          <i class="fas fa-toggle-on"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded manage-users"
                          data-deviceno="${data.deviceno}"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="管理用户">
                          <i class="fas fa-users"></i>
                        </button>
                        <button class="btn btn-outline-danger btn-rounded delete-device"
                          data-deviceno="${data.deviceno}"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="删除设备">
                          <i class="fas fa-trash"></i>
                        </button>
                    `;
                }
            }
        ],
        order: [[5, 'desc']] // 按创建时间降序排序
    });

    // 编辑设备按钮点击事件
    $('#table-device').on('click', '.edit-device', function() {
        // 获取当前行数据
        const row = $(this).closest('tr');
        const rowData = deviceTable.row(row).data();

        if (rowData && rowData.deviceno) {
            loadDeviceDetail(String(rowData.deviceno));
        } else {
            // 尝试从按钮的 data 属性获取
            const deviceNo = $(this).data('deviceno');
            if (deviceNo) {
                loadDeviceDetail(String(deviceNo));
            } else {
                showWarningToast('无法获取设备信息！');
            }
        }
    });

    // 删除设备按钮点击事件
    $('#table-device').on('click', '.delete-device', function() {
        // 获取当前行数据
        const row = $(this).closest('tr');
        const rowData = deviceTable.row(row).data();

        if (rowData && rowData.deviceno) {
            delType = 'device';
            delId = String(rowData.deviceno);
            $('#del-confirm').modal('show');
        } else {
            // 尝试从按钮的 data 属性获取
            const deviceNo = $(this).data('deviceno');
            if (deviceNo) {
                delType = 'device';
                delId = String(deviceNo);
                $('#del-confirm').modal('show');
            } else {
                showWarningToast('无法获取设备信息！');
            }
        }
    });

    // 管理用户按钮点击事件
    $('#table-device').on('click', '.manage-users', function() {
        // 获取当前行数据
        const row = $(this).closest('tr');
        const rowData = deviceTable.row(row).data();

        if (rowData && rowData.deviceno) {
            const deviceNo = String(rowData.deviceno);
            currentDeviceNo = deviceNo;
            $('#device-users-deviceNo').text(deviceNo);
            loadDeviceUsers(deviceNo);
            $('#modal-device-users').modal('show');
        } else {
            // 尝试从按钮的 data 属性获取
            const deviceNo = $(this).data('deviceno');
            if (deviceNo) {
                currentDeviceNo = String(deviceNo);
                $('#device-users-deviceNo').text(deviceNo);
                loadDeviceUsers(String(deviceNo));
                $('#modal-device-users').modal('show');
            } else {
                showWarningToast('无法获取设备信息！');
            }
        }
    });

    // 启用/禁用设备按钮点击事件
    $('#table-device').on('click', '.toggle-device', function() {
        // 获取当前行数据
        const row = $(this).closest('tr');
        const rowData = deviceTable.row(row).data();

        if (rowData && rowData.deviceno) {
            toggleDeviceStatus(String(rowData.deviceno));
        } else {
            // 尝试从按钮的 data 属性获取
            const deviceNo = $(this).data('deviceno');
            if (deviceNo) {
                toggleDeviceStatus(String(deviceNo));
            } else {
                showWarningToast('无法获取设备信息！');
            }
        }
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
            } else if (response && response.data) {
                if (response.data.length === 0) {
                    showWarningToast('未查询到设备数据！');
                } else {
                    const processedData = response.data.map(item => {
                        if (item.deviceno !== undefined) {
                            item.deviceno = String(item.deviceno);
                        }
                        return item;
                    });
                    deviceTable.clear().rows.add(processedData).draw();
                }
            } else if (Array.isArray(response)) {
                if (response.length === 0) {
                    showWarningToast('未查询到设备数据！');
                } else {
                    const processedData = response.map(item => {
                        if (item.deviceno !== undefined) {
                            item.deviceno = String(item.deviceno);
                        }
                        return item;
                    });
                    deviceTable.clear().rows.add(processedData).draw();
                }
            } else {
                showWarningToast('数据格式错误！');
                console.error('未知的响应格式:', response);
            }
        },
        error: function(xhr, _, error) {
            try {
                const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                    showWarningToast(errorResponse[0].Alert);
                } else if (errorResponse && errorResponse.message) {
                    showWarningToast(errorResponse.message);
                } else {
                    showWarningToast('加载设备数据失败: ' + error);
                }
            } catch (e) {
                showWarningToast('加载设备数据失败: ' + error);
            }
        }
    });
}

/**
 * 加载设备详情
 * @param {string} deviceNo - 设备号
 */
function loadDeviceDetail(deviceNo) {
    try {
        if (!deviceNo) {
            showWarningToast('设备号不能为空！');
            return;
        }

        // 直接从服务器获取设备详情，而不是从表格中获取
        $.ajax({
            url: '/device',
            type: 'GET',
            data: {
                deviceNo: deviceNo
            },
            success: function(response) {
                let deviceData = null;

                // 处理不同格式的响应
                if (response && response.data && Array.isArray(response.data) && response.data.length > 0) {
                    deviceData = response.data[0];
                } else if (Array.isArray(response) && response.length > 0) {
                    deviceData = response[0];
                }

                if (deviceData) {
                    // 设置编辑表单的值
                    $('#edit-deviceNo').val(deviceData.deviceno);
                    $('#edit-imei').val(deviceData.imei || '');
                    $('#edit-simNo').val(deviceData.simno || '');
                    $('#edit-remark').val(deviceData.remark || '');
                    $('#edit-enable').prop('checked', deviceData.enable === true);

                    // 显示编辑模态框
                    $('#modal-device-edit').modal('show');
                } else {
                    showWarningToast('未找到设备详情！');
                }
            },
            error: function(xhr, _, error) {
                try {
                    const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                    if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                        showWarningToast(errorResponse[0].Alert);
                    } else if (errorResponse && errorResponse.message) {
                        showWarningToast(errorResponse.message);
                    } else {
                        showWarningToast('加载设备详情失败: ' + error);
                    }
                } catch (e) {
                    showWarningToast('加载设备详情失败: ' + error);
                }
            }
        });
    } catch (error) {
        showWarningToast('加载设备详情失败！');
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
        error: function(xhr, _, error) {
            try {
                const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                    showWarningToast(errorResponse[0].Alert);
                } else if (errorResponse && errorResponse.message) {
                    showWarningToast(errorResponse.message);
                } else {
                    showWarningToast('添加设备失败: ' + error);
                }
            } catch (e) {
                showWarningToast('添加设备失败: ' + error);
            }
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
        error: function(xhr, _, error) {
            try {
                const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                    showWarningToast(errorResponse[0].Alert);
                } else if (errorResponse && errorResponse.message) {
                    showWarningToast(errorResponse.message);
                } else {
                    showWarningToast('更新设备失败: ' + error);
                }
            } catch (e) {
                showWarningToast('更新设备失败: ' + error);
            }
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
        error: function(xhr, _, error) {
            try {
                const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                    showWarningToast(errorResponse[0].Alert);
                } else if (errorResponse && errorResponse.message) {
                    showWarningToast(errorResponse.message);
                } else {
                    showWarningToast('删除设备失败: ' + error);
                }
            } catch (e) {
                showWarningToast('删除设备失败: ' + error);
            }
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
                try {
                    let userData = Array.isArray(response) ? response :
                                  (response && response.data && Array.isArray(response.data) ? response.data : []);

                    if (userData.length === 0) {
                        showWarningToast('该设备未分配用户！');
                        $('#device-users-body').html('');
                        return;
                    }

                    userData.forEach(function(user) {
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
                } catch (error) {
                    showWarningToast('处理设备用户数据失败！');
                    $('#device-users-body').html('');
                }
            }
        },
        error: function(xhr, _, error) {
            try {
                const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                    showWarningToast(errorResponse[0].Alert);
                } else if (errorResponse && errorResponse.message) {
                    showWarningToast(errorResponse.message);
                } else {
                    showWarningToast('加载设备用户失败: ' + error);
                }
            } catch (e) {
                showWarningToast('加载设备用户失败: ' + error);
            }
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
                let userData = Array.isArray(response) ? response :
                              (response && response.data && Array.isArray(response.data) ? response.data : []);

                userData.forEach(function(user) {
                    html += '<option value="' + user.id + '" data-loginname="' + user.loginname + '">' +
                        user.username + ' (' + user.loginname + ')</option>';
                });
                $('#user-select').html(html);
            }
        },
        error: function(xhr, _, error) {
            try {
                const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                    showWarningToast(errorResponse[0].Alert);
                } else if (errorResponse && errorResponse.message) {
                    showWarningToast(errorResponse.message);
                } else {
                    showWarningToast('加载用户列表失败: ' + error);
                }
            } catch (e) {
                showWarningToast('加载用户列表失败: ' + error);
            }
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
            // 关闭模态框
            $('#modal-add-device-user').modal('hide');

            // 根据返回信息显示不同的提示
            if (response && response.length > 0 && response[0].Alert) {
                const alertMessage = response[0].Alert;

                // 检查是否包含"成功"关键字
                if (alertMessage.includes('成功')) {
                    showSuccessToast(alertMessage);
                } else {
                    // 显示API返回的错误信息
                    showWarningToast(alertMessage);
                }

                // 无论成功还是失败，都刷新用户列表，因为操作可能已经成功
                setTimeout(function() {
                    loadDeviceUsers(currentDeviceNo);
                }, 500);
            } else {
                // 没有明确的消息，显示通用成功信息
                showSuccessToast('操作已完成');
                loadDeviceUsers(currentDeviceNo);
            }
        },
        error: function(xhr, _, error) {
            // 尝试解析错误响应
            try {
                const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                    showWarningToast(errorResponse[0].Alert);
                } else if (errorResponse && errorResponse.message) {
                    showWarningToast(errorResponse.message);
                } else {
                    showWarningToast('分配设备失败: ' + error);
                }
            } catch (e) {
                showWarningToast('分配设备失败: ' + error);
            }
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
                const alertMessage = response[0].Alert;

                // 检查是否包含"成功"关键字
                if (alertMessage.includes('成功')) {
                    showSuccessToast(alertMessage);
                } else {
                    // 显示API返回的错误信息
                    showWarningToast(alertMessage);
                }

                // 无论成功还是失败，都刷新用户列表，因为操作可能已经成功
                setTimeout(function() {
                    loadDeviceUsers(currentDeviceNo);
                }, 500);
            } else {
                // 没有明确的消息，显示通用成功信息
                showSuccessToast('操作已完成');
                loadDeviceUsers(currentDeviceNo);
            }
        },
        error: function(xhr, _, error) {
            try {
                const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                    showWarningToast(errorResponse[0].Alert);
                } else if (errorResponse && errorResponse.message) {
                    showWarningToast(errorResponse.message);
                } else {
                    showWarningToast('取消分配设备失败: ' + error);
                }
            } catch (e) {
                showWarningToast('取消分配设备失败: ' + error);
            }
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

/**
 * 切换设备状态（启用/禁用）
 * @param {string} deviceNo - 设备号
 */
function toggleDeviceStatus(deviceNo) {
    // 显示加载中提示
    showInfoToast('正在处理，请稍候...');

    $.ajax({
        url: '/device/toggle',
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify({
            deviceNo: deviceNo
        }),
        timeout: 10000, // 10秒超时
        success: function(response) {
            if (response && response.length > 0 && response[0].Alert) {
                const alertMessage = response[0].Alert;

                // 检查是否包含"成功"关键字
                if (alertMessage.includes('成功')) {
                    showSuccessToast(alertMessage);
                } else {
                    // 显示API返回的具体错误信息
                    showErrorToast(alertMessage);
                }

                // 刷新设备列表
                loadDeviceData();
            } else {
                // 没有明确的消息，显示通用成功信息
                showSuccessToast('操作已完成');
                loadDeviceData();
            }
        },
        error: function(xhr, textStatus, error) {
            try {
                // 处理不同类型的错误
                if (textStatus === 'timeout') {
                    showErrorToast('请求超时，请检查网络连接后重试');
                } else if (xhr.status === 0) {
                    showErrorToast('无法连接到服务器，请检查网络连接');
                } else if (xhr.status === 403) {
                    showErrorToast('权限不足，无法执行此操作');
                } else if (xhr.status === 404) {
                    // 404错误特殊处理，提供更详细的信息
                    showErrorToast('API路径错误，请联系管理员');
                } else if (xhr.status === 400) {
                    showErrorToast('请求参数错误，请检查设备号格式');
                } else if (xhr.status === 500) {
                    showErrorToast('服务器内部错误，请联系管理员');
                } else {
                    // 尝试解析错误响应
                    const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                    if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                        showErrorToast(errorResponse[0].Alert);
                    } else if (errorResponse && errorResponse.message) {
                        showErrorToast(errorResponse.message);
                    } else {
                        showErrorToast('切换设备状态失败: ' + error);
                    }
                }
            } catch (e) {
                showErrorToast('切换设备状态失败: ' + error);
            } finally {
                // 无论如何都刷新设备列表，确保显示最新状态
                loadDeviceData();
            }
        }
    });
}

// 这些函数已经在utils.js中定义，这里不再重复定义
