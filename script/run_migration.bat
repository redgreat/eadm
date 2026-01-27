@echo off
echo 正在运行sports权限迁移脚本...
curl -X GET "http://localhost:8080/sys/migrate/sports_permission" -H "Content-Type: application/json"
echo.
echo 迁移完成！
pause
