#!/bin/sh
# SSL证书测试脚本
# 用于测试证书配置和验证

set -e

CERT_DIR="${SSL_CERT_DIR:-/opt/eadm/certs}"
DOMAIN="${SSL_DOMAIN:-example.com}"

echo "=========================================="
echo "SSL证书测试工具"
echo "=========================================="

# 检查证书文件是否存在
check_cert_files() {
    echo ""
    echo "1. 检查证书文件..."
    
    if [ -f "$CERT_DIR/cert.pem" ]; then
        echo "✓ 证书文件存在: $CERT_DIR/cert.pem"
    else
        echo "✗ 证书文件不存在: $CERT_DIR/cert.pem"
        return 1
    fi
    
    if [ -f "$CERT_DIR/key.pem" ]; then
        echo "✓ 私钥文件存在: $CERT_DIR/key.pem"
    else
        echo "✗ 私钥文件不存在: $CERT_DIR/key.pem"
        return 1
    fi
}

# 查看证书详细信息
show_cert_info() {
    echo ""
    echo "2. 证书详细信息..."
    echo ""
    
    if [ -f "$CERT_DIR/cert.pem" ]; then
        echo "主题信息:"
        openssl x509 -in "$CERT_DIR/cert.pem" -noout -subject
        
        echo ""
        echo "颁发者:"
        openssl x509 -in "$CERT_DIR/cert.pem" -noout -issuer
        
        echo ""
        echo "有效期:"
        openssl x509 -in "$CERT_DIR/cert.pem" -noout -dates
        
        echo ""
        echo "域名:"
        openssl x509 -in "$CERT_DIR/cert.pem" -noout -text | grep -A1 "Subject Alternative Name"
    fi
}

# 检查证书有效期
check_cert_validity() {
    echo ""
    echo "3. 检查证书有效期..."
    
    if [ -f "$CERT_DIR/cert.pem" ]; then
        expiry_date=$(openssl x509 -enddate -noout -in "$CERT_DIR/cert.pem" | cut -d= -f2)
        expiry_epoch=$(date -d "$expiry_date" +%s 2>/dev/null || date -j -f "%b %d %T %Y %Z" "$expiry_date" +%s 2>/dev/null)
        current_epoch=$(date +%s)
        days_left=$(( ($expiry_epoch - $current_epoch) / 86400 ))
        
        echo "证书剩余有效期: $days_left 天"
        
        if [ $days_left -le 30 ]; then
            echo "⚠ 警告: 证书即将过期，建议续期"
        elif [ $days_left -le 0 ]; then
            echo "✗ 错误: 证书已过期"
        else
            echo "✓ 证书有效"
        fi
    fi
}

# 验证证书和私钥匹配
verify_cert_key_match() {
    echo ""
    echo "4. 验证证书和私钥匹配..."
    
    if [ -f "$CERT_DIR/cert.pem" ] && [ -f "$CERT_DIR/key.pem" ]; then
        cert_md5=$(openssl x509 -noout -modulus -in "$CERT_DIR/cert.pem" | openssl md5)
        key_md5=$(openssl rsa -noout -modulus -in "$CERT_DIR/key.pem" 2>/dev/null | openssl md5)
        
        if [ "$cert_md5" = "$key_md5" ]; then
            echo "✓ 证书和私钥匹配"
        else
            echo "✗ 证书和私钥不匹配"
            return 1
        fi
    fi
}

# 测试HTTPS连接
test_https_connection() {
    echo ""
    echo "5. 测试HTTPS连接..."
    
    if command -v curl > /dev/null; then
        echo "测试连接到 https://$DOMAIN:8443"
        if curl -k -s -o /dev/null -w "%{http_code}" "https://$DOMAIN:8443" > /dev/null 2>&1; then
            echo "✓ HTTPS服务可访问"
        else
            echo "⚠ HTTPS服务不可访问（可能服务未启动）"
        fi
    else
        echo "⚠ curl未安装，跳过连接测试"
    fi
}

# 查看acme.sh状态
check_acme_status() {
    echo ""
    echo "6. acme.sh 状态..."
    
    if [ -d "/root/.acme.sh" ]; then
        echo "✓ acme.sh 已安装"
        
        if [ -f "/root/.acme.sh/acme.sh.env" ]; then
            . /root/.acme.sh/acme.sh.env
            
            echo ""
            echo "已注册的证书:"
            /root/.acme.sh/acme.sh --list 2>/dev/null || echo "无证书记录"
        fi
    else
        echo "✗ acme.sh 未安装"
    fi
}

# 主函数
main() {
    check_cert_files || true
    show_cert_info || true
    check_cert_validity || true
    verify_cert_key_match || true
    test_https_connection || true
    check_acme_status || true
    
    echo ""
    echo "=========================================="
    echo "测试完成"
    echo "=========================================="
}

main
