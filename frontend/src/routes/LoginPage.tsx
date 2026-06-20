import { useNavigate } from "@solidjs/router";
import { createSignal, Show } from "solid-js";
import Button from "../components/ui/Button";
import { login } from "../lib/api/auth";

export default function LoginPage() {
  const navigate = useNavigate();
  const [loginName, setLoginName] = createSignal("");
  const [password, setPassword] = createSignal("");
  const [message, setMessage] = createSignal("");
  const [submitting, setSubmitting] = createSignal(false);

  const handleSubmit = async (event: SubmitEvent) => {
    event.preventDefault();
    setSubmitting(true);
    setMessage("");

    try {
      const result = await login(loginName(), password());
      if (result.success) {
        navigate("/", { replace: true });
        return;
      }
      setMessage(result.message || "登录失败");
    } catch {
      setMessage("网络错误，请稍后重试");
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <main class="grid min-h-screen place-items-center bg-slate-950 px-4">
      <section class="w-full max-w-sm rounded-lg bg-white p-6 shadow-xl">
        <div>
          <h1 class="text-2xl font-semibold tracking-tight">登录 EADM</h1>
          <p class="mt-1 text-sm text-slate-500">使用现有账号登录新 SolidJS 前端。</p>
        </div>

        <form class="mt-6 space-y-4" onSubmit={handleSubmit}>
          <label class="block text-sm font-medium">
            登录名
            <input
              value={loginName()}
              onInput={(event) => setLoginName(event.currentTarget.value)}
              autocomplete="username"
              class="mt-1 h-10 w-full rounded-md border border-slate-300 px-3 outline-none focus:border-slate-950"
            />
          </label>
          <label class="block text-sm font-medium">
            密码
            <input
              type="password"
              value={password()}
              onInput={(event) => setPassword(event.currentTarget.value)}
              autocomplete="current-password"
              class="mt-1 h-10 w-full rounded-md border border-slate-300 px-3 outline-none focus:border-slate-950"
            />
          </label>
          <Show when={message()}>
            <div class="rounded-md bg-amber-50 px-3 py-2 text-sm text-amber-700">{message()}</div>
          </Show>
          <Button class="w-full" type="submit" disabled={submitting()}>
            {submitting() ? "登录中..." : "登录"}
          </Button>
        </form>
      </section>
    </main>
  );
}
