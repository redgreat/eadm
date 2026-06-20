import { apiRequest } from "./client";
import type { ApiResponse } from "./client";

export type UserItem = {
  id: number | string;
  tenantName: string;
  loginName: string;
  userName: string;
  email: string;
  userStatus: number;
  createdAt: string;
};

export type UserList = {
  items: UserItem[];
  total: number;
};

export function getUsers(): Promise<ApiResponse<UserList>> {
  return apiRequest<UserList>("/api/users");
}
