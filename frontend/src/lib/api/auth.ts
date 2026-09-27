import { apiRequest, apiV1 } from "./client";
import type { ApiResponse } from "./client";

export type CurrentUser = {
  authed: boolean;
  loginName: string;
  userName: string;
  permission: Record<string, unknown>;
};

export function getCurrentUser(): Promise<ApiResponse<CurrentUser>> {
  return apiRequest<CurrentUser>(`${apiV1}/auth/me`);
}

export function login(loginName: string, password: string): Promise<ApiResponse<CurrentUser>> {
  const body = new URLSearchParams();
  body.set("loginName", loginName);
  body.set("password", password);

  return apiRequest<CurrentUser>(`${apiV1}/auth/login`, {
    method: "POST",
    body
  });
}

export function logout(): Promise<ApiResponse<Record<string, never>>> {
  return apiRequest<Record<string, never>>(`${apiV1}/auth/logout`, {
    method: "POST"
  });
}
