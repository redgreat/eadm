import { expect, test } from "@playwright/test";

const ok = (data: unknown, message = "") => ({ success: true, code: "ok", message, data });

test("shows a login error returned by the API", async ({ page }) => {
  await page.route("**/api/v1/auth/login", (route) => route.fulfill({
    status: 401,
    contentType: "application/json",
    body: JSON.stringify({ success: false, code: "unauthorized", message: "账号或密码错误", data: {} })
  }));
  await page.goto("/login");
  await page.getByLabel("登录名").fill("admin");
  await page.getByLabel("密码").fill("bad-password");
  await page.getByRole("button", { name: "登录", exact: true }).click();
  await expect(page.getByText("账号或密码错误")).toBeVisible();
});

test("renders an authenticated dashboard and supports theme switching", async ({ page }) => {
  await page.route("**/api/v1/auth/me", (route) => route.fulfill({
    contentType: "application/json",
    body: JSON.stringify(ok({ authed: true, loginName: "admin", userName: "管理员", permission: {} }))
  }));
  await page.route("**/api/v1/dashboard/summary", (route) => route.fulfill({
    contentType: "application/json",
    body: JSON.stringify(ok({
      cards: { health: "12", location: "34", financeIncome: "56.00", financeExpense: "7.00" },
      locationTrend: { labels: ["周一"], values: ["34"] },
      financeTrend: { labels: ["周一"], income: ["56"], expense: ["7"] }
    }))
  }));
  await page.goto("/");
  await expect(page.getByRole("heading", { name: "仪表盘" })).toBeVisible();
  await expect(page.getByText("管理员")).toBeVisible();
  await expect(page.getByText("56.00")).toBeVisible();
  await page.getByTitle("切换主题").click();
  await expect(page.locator("html")).not.toHaveClass(/dark/);
});
