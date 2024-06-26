# @author wangcw
# @copyright (C) 2024, REDGREAT
# Created : 2024-04-09 下午1:48

DROP EVENT IF EXISTS ev_DashBoard;
CREATE EVENT ev_DashBoard
  ON SCHEDULE EVERY 1 DAY
  ON COMPLETION NOT PRESERVE
  ENABLE
  COMMENT '定时任务_每日首页报表统计'
  DO BEGIN
CALL proc_DashBoard(CURDATE());
END;
