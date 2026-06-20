export type ApiResponse<T> = {
  success: boolean;
  code: string;
  message: string;
  data: T;
};

const apiBase = import.meta.env.VITE_API_BASE ?? "";

type RequestOptions = RequestInit & {
  skipJsonParse?: boolean;
};

export async function apiRequest<T>(input: string, options: RequestOptions = {}): Promise<ApiResponse<T>> {
  const hasJsonBody = options.body !== undefined && !(options.body instanceof URLSearchParams);
  const url = input.startsWith("http") ? input : `${apiBase}${input}`;

  const response = await fetch(url, {
    credentials: "include",
    headers: {
      Accept: "application/json",
      ...(hasJsonBody ? { "Content-Type": "application/json" } : {}),
      ...options.headers
    },
    ...options
  });

  if (options.skipJsonParse) {
    return {
      success: response.ok,
      code: response.ok ? "ok" : "http_error",
      message: response.statusText,
      data: undefined as T
    };
  }

  const contentType = response.headers.get("content-type") ?? "";
  if (!contentType.includes("application/json")) {
    return {
      success: false,
      code: response.status === 401 || response.url.endsWith("/login") ? "unauthorized" : "non_json_response",
      message: response.status === 401 || response.url.endsWith("/login") ? "请先登录" : "服务返回格式错误",
      data: undefined as T
    };
  }

  const body = (await response.json()) as ApiResponse<T>;

  if (!response.ok && body.success) {
    return {
      ...body,
      success: false,
      code: body.code || "http_error",
      message: body.message || response.statusText
    };
  }

  return body;
}
