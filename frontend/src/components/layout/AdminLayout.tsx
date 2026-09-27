import { A, useLocation, useNavigate } from "@solidjs/router";
import {
  Activity,
  CalendarClock,
  ChevronDown,
  Gauge,
  HeartPulse,
  KeyRound,
  LogOut,
  MapPinned,
  Menu,
  Moon,
  ServerCog,
  Shield,
  Sun,
  UserRound,
  UsersRound,
  X
} from "lucide-solid";
import type { ParentProps } from "solid-js";
import { createEffect, createMemo, createResource, createSignal, For, onCleanup, Show } from "solid-js";
import Button from "../ui/Button";
import { getCurrentUser, logout } from "../../lib/api/auth";
import { cn } from "../../lib/cn";

const navItems = [
  { href: "/", label: "信息看板", icon: Gauge },
  { href: "/health", label: "我的健康", icon: HeartPulse },
  { href: "/location", label: "轨迹回放", icon: MapPinned },
  { href: "/finance", label: "我的财务", icon: Activity },
  { href: "/crontab", label: "定时任务", icon: CalendarClock },
  { href: "/user", label: "用户信息", icon: UsersRound },
  { href: "/role", label: "角色权限", icon: Shield },
  { href: "/device", label: "设备管理", icon: UserRound },
  { href: "/sysinfo", label: "系统信息", icon: ServerCog }
];

const titles: Record<string, string> = {
  "/": "信息看板",
  "/health": "我的健康",
  "/location": "轨迹回放",
  "/finance": "我的财务",
  "/crontab": "定时任务",
  "/user": "用户信息",
  "/role": "角色权限",
  "/device": "设备管理",
  "/sysinfo": "系统信息"
};

export default function AdminLayout(props: ParentProps) {
  const navigate = useNavigate();
  const location = useLocation();
  const [session] = createResource(getCurrentUser);
  const [collapsed, setCollapsed] = createSignal(true);
  const [accountOpen, setAccountOpen] = createSignal(false);
  const [modal, setModal] = createSignal<"profile" | "password" | null>(null);
  const [toast, setToast] = createSignal("");
  const [theme, setTheme] = createSignal(localStorage.getItem("eadm-theme") ?? "dark");

  createEffect(() => {
    const result = session();
    if (result && !result.success && result.code === "unauthorized") {
      navigate("/login", { replace: true });
    }
  });

  createEffect(() => {
    const dark = theme() === "dark";
    document.documentElement.classList.toggle("dark", dark);
    localStorage.setItem("eadm-theme", theme());
  });

  createEffect(() => {
    const handleScroll = () => {
      if (window.scrollY > 10) {
        setCollapsed(true);
      }
    };
    window.addEventListener("scroll", handleScroll, { passive: true });
    onCleanup(() => window.removeEventListener("scroll", handleScroll));
  });

  const pageTitle = createMemo(() => titles[location.pathname] ?? "页面不存在");
  const userName = createMemo(() => session()?.data.userName || session()?.data.loginName || "未登录");
  const loginName = createMemo(() => session()?.data.loginName || "");

  const showToast = (message: string) => {
    setToast(message);
    window.setTimeout(() => setToast(""), 5000);
  };

  const handleLogout = async () => {
    await logout();
    window.location.href = "/login";
  };

  const submitPending = () => {
    setModal(null);
    showToast("当前操作接口还没有迁移到 Cowboy，前端菜单已保留入口。");
  };

  return (
    <div class="min-h-screen bg-slate-100 text-slate-950 dark:bg-slate-950 dark:text-slate-100">
      <aside
        class={cn(
          "fixed inset-y-0 left-0 z-30 w-[250px] border-r border-slate-200 bg-white shadow-sm transition-transform duration-300 dark:border-slate-800 dark:bg-slate-900",
          collapsed() && "-translate-x-full"
        )}
      >
        <div class="flex h-[55px] items-center border-b border-slate-200 bg-sky-50 px-4 dark:border-slate-800 dark:bg-slate-900">
          <img src="/assets/img/redgreat-header.png" alt="EADM" class="h-9 w-auto" />
        </div>
        <nav class="space-y-1 p-2 text-slate-600 dark:text-slate-300">
          <For each={navItems}>
            {(item) => {
              const Icon = item.icon;
              return (
                <A
                  href={item.href}
                  class="flex items-center gap-3 rounded-md px-4 py-3 text-sm font-medium hover:bg-sky-50 hover:text-sky-600 dark:hover:bg-slate-800 dark:hover:text-white"
                  activeClass="bg-sky-500 text-white hover:bg-sky-500 hover:text-white dark:bg-sky-600"
                  end={item.href === "/"}
                >
                  <Icon size={18} />
                  {item.label}
                </A>
              );
            }}
          </For>
        </nav>
      </aside>

      <div class={cn("min-h-screen transition-[margin] duration-300", collapsed() ? "ml-0" : "ml-[250px]")}>
        <header class="fixed left-0 right-0 top-0 z-20 flex h-[60px] items-center justify-between border-b border-slate-200 bg-white px-4 shadow-sm dark:border-slate-800 dark:bg-slate-900 lg:px-6">
          <div class="flex min-w-0 items-center gap-3">
            <button
              type="button"
              class="inline-flex h-10 w-10 items-center justify-center rounded-md bg-slate-100 text-slate-700 hover:bg-slate-200 dark:bg-slate-800 dark:text-slate-100 dark:hover:bg-slate-700"
              onClick={() => setCollapsed((value) => !value)}
              title="收缩菜单"
            >
              <Menu size={18} />
            </button>
            <nav class="min-w-0 text-sm text-slate-500 dark:text-slate-400">
              <ol class="flex min-w-0 items-center gap-2">
                <li class="hidden sm:inline">EADM</li>
                <li class="hidden sm:inline text-slate-300">/</li>
                <li class="max-w-[42vw] truncate font-medium text-slate-950 dark:text-slate-100">{pageTitle()}</li>
              </ol>
            </nav>
          </div>

          <div class="flex items-center gap-2">
            <button
              type="button"
              class="inline-flex h-10 w-10 items-center justify-center rounded-md text-slate-600 hover:bg-slate-100 dark:text-slate-200 dark:hover:bg-slate-800"
              title="切换主题"
              onClick={() => setTheme((value) => (value === "dark" ? "light" : "dark"))}
            >
              <Show when={theme() === "dark"} fallback={<Moon size={18} />}>
                <Sun size={18} />
              </Show>
            </button>

            <div class="relative">
              <button
                type="button"
                class="inline-flex h-10 items-center gap-2 rounded-md px-3 text-sm text-slate-600 hover:bg-slate-100 dark:text-slate-200 dark:hover:bg-slate-800"
                onClick={() => setAccountOpen((value) => !value)}
              >
                <UserRound size={17} />
                <span class="hidden sm:inline">{userName()}</span>
                <ChevronDown size={14} />
              </button>
              <Show when={accountOpen()}>
                <div class="absolute right-0 mt-2 w-48 rounded-md border border-slate-200 bg-white py-2 text-sm shadow-lg dark:border-slate-800 dark:bg-slate-900">
                  <button class="flex w-full items-center gap-2 px-4 py-2 text-left hover:bg-sky-50 hover:text-sky-600 dark:hover:bg-slate-800" onClick={() => { setModal("profile"); setAccountOpen(false); }}>
                    <UserRound size={16} /> 个人信息
                  </button>
                  <button class="flex w-full items-center gap-2 px-4 py-2 text-left hover:bg-sky-50 hover:text-sky-600 dark:hover:bg-slate-800" onClick={() => { setModal("password"); setAccountOpen(false); }}>
                    <KeyRound size={16} /> 修改密码
                  </button>
                  <div class="my-1 border-t border-slate-100 dark:border-slate-800" />
                  <button class="flex w-full items-center gap-2 px-4 py-2 text-left hover:bg-sky-50 hover:text-sky-600 dark:hover:bg-slate-800" onClick={handleLogout}>
                    <LogOut size={16} /> 退出
                  </button>
                </div>
              </Show>
            </div>
          </div>
        </header>

        <main class="mx-auto max-w-7xl px-4 pb-24 pt-[76px] lg:px-6">{props.children}</main>
        <footer class="pb-6 text-center text-xs text-slate-400">
          Copyright © wangcw 2020-{new Date().getFullYear()} All Rights Reserved
        </footer>
      </div>

      <Show when={modal() === "profile"}>
        <Modal title="用户信息" onClose={() => setModal(null)}>
          <Field label="登录名" value={loginName()} readonly />
          <Field label="显示姓名" value={userName()} readonly />
          <Field label="邮箱" value="" readonly />
          <div class="mt-6 flex justify-end gap-2">
            <Button variant="secondary" type="button" onClick={() => showToast("个人信息编辑接口待迁移。")}>编辑</Button>
            <Button type="button" onClick={submitPending}>提交</Button>
            <Button variant="ghost" type="button" onClick={() => setModal(null)}>关闭</Button>
          </div>
        </Modal>
      </Show>

      <Show when={modal() === "password"}>
        <Modal title="密码修改" onClose={() => setModal(null)}>
          <Field label="旧密码" type="password" />
          <Field label="新密码" type="password" />
          <Field label="确认密码" type="password" />
          <div class="mt-6 flex justify-end gap-2">
            <Button type="button" onClick={submitPending}>提交</Button>
            <Button variant="ghost" type="button" onClick={() => setModal(null)}>关闭</Button>
          </div>
        </Modal>
      </Show>

      <Show when={toast()}>
        <div class="fixed bottom-4 right-4 z-50 w-80 rounded-md border border-sky-100 bg-white p-4 text-sm shadow-lg dark:border-slate-800 dark:bg-slate-900">
          <div class="font-medium text-slate-950 dark:text-slate-100">系统消息通知</div>
          <div class="mt-1 text-slate-600 dark:text-slate-300">{toast()}</div>
        </div>
      </Show>
    </div>
  );
}

function Modal(props: ParentProps<{ title: string; onClose: () => void }>) {
  return (
    <div class="fixed inset-0 z-40 flex items-center justify-center bg-slate-950/60 px-4">
      <section class="w-full max-w-lg rounded-lg border border-slate-200 bg-white p-5 shadow-xl dark:border-slate-800 dark:bg-slate-900">
        <div class="flex items-center justify-between border-b border-slate-100 pb-3 dark:border-slate-800">
          <h3 class="text-lg font-semibold">{props.title}</h3>
          <button type="button" class="rounded-md p-1 text-slate-500 hover:bg-slate-100 dark:hover:bg-slate-800" onClick={props.onClose}>
            <X size={18} />
          </button>
        </div>
        <div class="pt-5">{props.children}</div>
      </section>
    </div>
  );
}

function Field(props: { label: string; value?: string; type?: string; readonly?: boolean }) {
  return (
    <label class="mb-3 grid gap-2 text-sm sm:grid-cols-[96px_1fr] sm:items-center">
      <span class="text-slate-500 dark:text-slate-400">{props.label}</span>
      <input
        type={props.type ?? "text"}
        value={props.value ?? ""}
        readonly={props.readonly}
        class="h-10 rounded-md border border-slate-200 bg-white px-3 text-slate-950 outline-none focus:border-sky-500 dark:border-slate-700 dark:bg-slate-950 dark:text-slate-100"
      />
    </label>
  );
}
