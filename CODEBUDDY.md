# freebox.el — CodeBuddy 项目记忆

## 项目概述

freebox.el 是一个 Emacs Lisp 包，为 FreeBox（CATVOD Spider 兼容后端）提供
树形 buffer 浏览/播放界面（`*freebox-vod*` 点播树、`*freebox-live*` 直播树），
通过 empv 调用 mpv 播放视频。旧的 `completing-read` 链已收缩为
海报/图集（freebox-image）与扫码登录等侧路流程。

## 文件结构

| 文件 | 职责 |
|------|------|
| `freebox.el` | 包入口，汇总 require |
| `freebox-http.el` | HTTP 客户端，封装 `/api/*` 请求 |
| `freebox-persist.el` | 持久化层，使用 `~/.emacs.d/freebox-state.el` 存储状态 |
| `freebox-ui.el` | 公共 UI 助手 + 海报/图集/扫码等旧链侧路流程 |
| `freebox-vod.el` | 点播树 buffer（分类→影片→线路→剧集，懒加载） |
| `freebox-live.el` | 直播树 buffer（分组→频道→线路） |
| `freebox-commands.el` | 对外暴露的 interactive 命令与 pretty-hydra 菜单（`v` 键） |
| `freebox-empv.el` | empv 集成，负责实际播放 |

## 代码约定

- 使用 `lexical-binding: t`
- json-read 返回 alist（symbol 键），统一用 `alist-get` 访问，包装为 `freebox-ui--jget`
- HTTP 回调风格：`(lambda (err data) ...)`
- 所有 completing-read 调用必须使用 `freebox-ui--completing-read`（已捕获 C-g quit signal）
- 提交遵循 Conventional Commits（feat/fix/refactor 等）

## v-cursor 导航记忆机制（已实现）

用户按 `v` 键可恢复到上次停留的导航节点。

### 节点层级（由浅到深）

```
category (1) → vod-list (2) → vod-detail (3) → episode (4)
```

### v-cursor 数据结构（存于 freebox-persist）

```elisp
;; category
((type . "category") (source-key . KEY) (tid . TID) (name . NAME))
;; vod-list
((type . "vod-list") (source-key . KEY) (tid . TID) (cat-name . NAME) (page . N))
;; vod-detail（tid/cat-name/page 由点播树写入；旧链省略，恢复时降级）
((type . "vod-detail") (source-key . KEY) (vod-id . ID) (vod-name . NAME)
 (tid . TID) (cat-name . NAME) (page . N))
;; episode（同上）
((type . "episode") (source-key . KEY) (vod-id . ID) (flag . FLAG)
 (tid . TID) (cat-name . NAME) (page . N))
```

### 恢复逻辑（freebox-vod-resume）

| v-cursor 类型 | 恢复到 |
|---|---|
| `episode` | 点播树：展开分类（串行补拉 1..N 页）→ 展开影片详情 → 展开线路，定位线路节点 |
| `vod-detail` | 点播树：展开分类 → 展开影片详情，定位影片节点 |
| `vod-list` | 点播树：展开分类并补拉到第 N 页，定位分类节点 |
| `category` | 点播树：展开该分类，定位分类节点 |
| nil / 源已切换 / 节点不存在 | 打开点播树 + message 降级到最近有效节点 |

### C-g 语义

点播树内无 completing-read，不存在 C-g 中断问题；旧链（海报/图集/扫码）
保留原语义：任何层级 C-g 静默取消且不写 v-cursor，只有真实选择才更新。

## 旧链「返回上一级」

常量 `freebox-ui--back-label = ".. (返回上一级)"`，仅存在于海报/图集侧路。
返回统一走 `freebox-ui--back-to-vod-tree`（回到点播树）；
选集列表的返回仍是重选播放源（`select-episode`）。

## 最近提交记录

| 提交 | 说明 |
|------|------|
| `73db131` | feat: 各菜单加入「返回上一级」，静默处理 C-g |
| `0b7f3b1` | fix: 修复 vod-detail/episode 节点未记忆问题 |
| `540435e` | fix: 修复 node-level 类型不匹配错误 |
| `a28a3b7` | feat: 实现 v-cursor 节点记忆与恢复机制 |

## 待规划方向（下次继续）

- [ ] **历史浏览记录入口**
  - `freebox-persist-add-history` 已有 clients/sources/categories 三类历史数据
  - 可增加 `H` 键入口，直接从历史记录跳转到某个分类/源
  - 需要在 `freebox-commands.el` 的 hydra 菜单中添加绑定

- [ ] **v 菜单状态行显示当前记忆位置**
  - 在 `freebox-ui-show-current-state` 里追加 v-cursor 节点信息
  - 例如：`[客户端] [源] > 分类名 p.3`
  - 让用户在按 v 前就能看到将要恢复的位置
