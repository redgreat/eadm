import { apiRequest } from "./client";
import type { ApiResponse } from "./client";

export type DeviceItem = {
  deviceNo: string;
  imei: string;
  simNo: string;
  remark: string;
  enable: boolean | number;
  createdAt: string;
};

export type DeviceList = {
  items: DeviceItem[];
  total: number;
};

export function getDevices(deviceNo = ""): Promise<ApiResponse<DeviceList>> {
  const params = new URLSearchParams();
  if (deviceNo.trim()) {
    params.set("deviceNo", deviceNo.trim());
  }
  const query = params.toString();
  return apiRequest<DeviceList>(query ? `/api/devices?${query}` : "/api/devices");
}
