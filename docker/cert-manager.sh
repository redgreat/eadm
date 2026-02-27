#!/bin/sh
# SSL证书自动管理脚本
# 功能：检查证书有效期，自动申请/续期证书

set -e

# ==================== 配置区 ====================
# 从环境变量读取配置（由Erlang模块设置）
DOMAIN="${SSL_DOMAIN:-example.com}"
EMAIL="${SSL_EMAIL:-admin@example.com}"
CERT_DIR="${SSL_CERT_DIR:-/opt/eadm/certs}"
ACME_HOME="/root/.acme.sh"
VALIDATION="${SSL_VALIDATION:-http}"
DNS_PROVIDER="${SSL_DNS_PROVIDER:-}"
DAYS_BEFORE_EXPIRE=30

# ==================== 日志函数 ====================
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"
}

error() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: $*" >&2
}

# ==================== 检查证书有效期 ====================
check_cert_expiry() {
    local cert_file="$1"
    
    if [ ! -f "$cert_file" ]; then
        log "证书文件不存在: $cert_file"
        return 1
    fi
    
    # 获取证书过期时间
    local expiry_date=$(openssl x509 -enddate -noout -in "$cert_file" | cut -d= -f2)
    local expiry_epoch=$(date -d "$expiry_date" +%s 2>/dev/null || date -j -f "%b %d %T %Y %Z" "$expiry_date" +%s 2>/dev/null)
    local current_epoch=$(date +%s)
    local days_left=$(( ($expiry_epoch - $current_epoch) / 86400 ))
    
    log "证书剩余有效期: $days_left 天"
    
    if [ $days_left -le $DAYS_BEFORE_EXPIRE ]; then
        log "证书即将过期或已过期，需要续期"
        return 1
    else
        log "证书仍然有效，无需续期"
        return 0
    fi
}

# ==================== 安装 acme.sh ====================
install_acme() {
    if [ -d "$ACME_HOME" ]; then
        log "acme.sh 已安装"
        return 0
    fi
    
    log "开始安装 acme.sh..."
    curl -s https://get.acme.sh | sh -s email="$EMAIL"
    
    if [ $? -eq 0 ]; then
        log "acme.sh 安装成功"
        return 0
    else
        error "acme.sh 安装失败"
        return 1
    fi
}

# ==================== 申请/续期证书 ====================
issue_or_renew_cert() {
    log "开始申请/续期证书: $DOMAIN"
    
    # 加载 acme.sh 环境
    . "$ACME_HOME/acme.sh.env"
    
    # 创建证书目录
    mkdir -p "$CERT_DIR"
    
    # 构建申请命令
    local issue_cmd="$ACME_HOME/acme.sh --issue"
    
    # 根据验证方式选择参数
    if [ "$VALIDATION" = "dns" ] && [ -n "$DNS_PROVIDER" ]; then
        log "使用 DNS 验证: $DNS_PROVIDER"
        issue_cmd="$issue_cmd --dns $DNS_PROVIDER -d $DOMAIN -d *.$DOMAIN"
    else
        log "使用 HTTP 验证"
        issue_cmd="$issue_cmd -d $DOMAIN --standalone --httpport 80"
    fi
    
    # 添加其他参数
    issue_cmd="$issue_cmd --keylength ec-256 --force"
    
    # 执行申请
    log "执行命令: $issue_cmd"
    eval $issue_cmd
    
    if [ $? -ne 0 ]; then
        error "证书申请失败"
        return 1
    fi
    
    # 安装证书到指定目录
    log "安装证书到 $CERT_DIR"
    $ACME_HOME/acme.sh --install-cert -d "$DOMAIN" \
        --key-file "$CERT_DIR/key.pem" \
        --fullchain-file "$CERT_DIR/cert.pem" \
        --reloadcmd "touch $CERT_DIR/.reload"
    
    if [ $? -eq 0 ]; then
        log "证书安装成功"
        chmod 644 "$CERT_DIR/cert.pem"
        chmod 600 "$CERT_DIR/key.pem"
        return 0
    else
        error "证书安装失败"
        return 1
    fi
}

# ==================== 重载应用 ====================
reload_app() {
    log "检查是否需要重载应用..."
    
    if [ -f "$CERT_DIR/.reload" ]; then
        log "证书已更新，尝试重载应用..."
        
        # 方式1: 如果应用支持热重载配置
        if [ -f "/opt/eadm/bin/eadm" ]; then
            /opt/eadm/bin/eadm eval 'application:reload_config()' 2>/dev/null || true
        fi
        
        # 方式2: 发送信号给进程（需要应用支持）
        # pkill -HUP -f eadm || true
        
        rm -f "$CERT_DIR/.reload"
        log "应用重载完成"
    fi
}

# ==================== 主函数 ====================
main() {
    log "=========================================="
    log "SSL证书管理脚本启动"
    log "域名: $DOMAIN"
    log "邮箱: $EMAIL"
    log "验证方式: $VALIDATION"
    log "证书目录: $CERT_DIR"
    log "=========================================="
    
    # 检查必要参数
    if [ "$DOMAIN" = "example.com" ]; then
        error "请在 sys.config 中设置 ssl_domain"
        exit 1
    fi
    
    # 检查证书是否需要续期
    if check_cert_expiry "$CERT_DIR/cert.pem"; then
        log "证书有效，无需操作"
        exit 0
    fi
    
    # 安装 acme.sh
    if ! install_acme; then
        error "acme.sh 安装失败，退出"
        exit 1
    fi
    
    # 申请或续期证书
    if issue_or_renew_cert; then
        log "证书操作成功"
        reload_app
        exit 0
    else
        error "证书操作失败"
        exit 1
    fi
}

# 执行主函数
main
