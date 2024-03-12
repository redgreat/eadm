CREATE TABLE eadm_user(
id INT AUTO_INCREMENT PRIMARY KEY,
un varchar(50) COMMENT '用户名',
upw varchar(50) COMMENT '密码',
inserttime TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) COMMENT '数据写入时间',
INDEX `NON-un`(un)
);

INSERT INTO `eadm_user` (`id`, `un`, `upw`, `inserttime`) VALUES
(1, 'wangcw', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=', NOW());
