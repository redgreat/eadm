// ========================================
// 运动数据设置页面JavaScript
// ========================================

// 关联Garmin账户
async function linkGarmin(event) {
    event.preventDefault();
    
    const email = document.getElementById('garminEmailInput').value;
    const password = document.getElementById('garminPasswordInput').value;
    
    try {
        const response = await fetch('/sports/settings/garmin/link', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({email, password})
        });
        
        const result = await response.json();
        
        if (result.code === 200) {
            alert('关联成功!');
            location.reload();
        } else {
            alert('关联失败: ' + result.message);
        }
    } catch (error) {
        alert('关联失败: ' + error.message);
    }
}

// 解除关联
async function unlinkGarmin() {
    if (!confirm('确定要解除Garmin账户关联吗?')) return;
    
    try {
        const response = await fetch('/sports/settings/garmin/unlink', {
            method: 'DELETE'
        });
        
        const result = await response.json();
        
        if (result.code === 200) {
            alert('已解除关联');
            location.reload();
        } else {
            alert('操作失败: ' + result.message);
        }
    } catch (error) {
        alert('操作失败: ' + error.message);
    }
}

// 更新同步配置
async function updateSyncConfig() {
    const config = {
        syncEnabled: true,
        autoSync: document.getElementById('autoSyncEnabled').checked,
        syncDays: parseInt(document.getElementById('syncDays').value)
    };
    
    try {
        const response = await fetch('/sports/settings/sync_config', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify(config)
        });
        
        const result = await response.json();
        
        if (result.code !== 200) {
            alert('更新失败: ' + result.message);
        }
    } catch (error) {
        console.error('更新失败:', error);
    }
}

// 触发手动同步
async function triggerManualSync() {
    const syncDays = parseInt(document.getElementById('syncDays').value);
    const statusSpan = document.getElementById('syncStatus');
    
    statusSpan.innerHTML = '<span class="text-primary"><i class="fas fa-spinner fa-spin"></i> 同步中...</span>';
    
    try {
        const response = await fetch('/sports/sync', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({daysBack: syncDays})
        });
        
        const result = await response.json();
        
        if (result.code === 200) {
            statusSpan.innerHTML = '<span class="text-success"><i class="fas fa-check"></i> 同步已开始</span>';
            setTimeout(() => {
                location.reload();
            }, 3000);
        } else {
            statusSpan.innerHTML = '<span class="text-danger"><i class="fas fa-times"></i> ' + result.message + '</span>';
        }
    } catch (error) {
        statusSpan.innerHTML = '<span class="text-danger"><i class="fas fa-times"></i> 同步失败</span>';
    }
}
