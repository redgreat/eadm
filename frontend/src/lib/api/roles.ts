import { apiRequest } from "./client";
import type { ApiResponse } from "./client";

export type RoleItem = {
  id: number | string;
  roleName: string;
  roleStatus: number;
  createdAt: string;
};

export type RoleList = {
  items: RoleItem[];
  total: number;
};

export function getRoles(): Promise<ApiResponse<RoleList>> {
  return apiRequest<RoleList>("/api/roles");
}
