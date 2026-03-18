# freebox.el — CodeBuddy 项目记忆

## 项目概述

freebox.el 是一个 Emacs Lisp 包，为 FreeBox（CATVOD Spider 兼容后端）提供
`completing-read` 风格的浏览/播放界面，通过 empv 调用 mpv 播放视频。

## 文件结构

| 文件 | 职责 |
|------|------|
| `freebox.el` | 包入口，汇总 require |
| `freebox-http.el` | HTTP 客户端，封装 `/api/*` 请求 |
| `freebox-persist.el` | 持久化层，使用 `~/.emacs.d/freebox-state.el` 存储状态 |
| `freebox-ui.el` | UI 层，所有 completing-read 菜单与导航逻辑 |
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
;; vod-detail
((type . "vod-detail") (source-key . KEY) (vod-id . ID) (vod-name . NAME))
;; episode
((type . "episode") (source-key . KEY) (vod-id . ID) (flag . FLAG))
```

### 恢复逻辑（freebox-ui-resume）

| v-cursor 类型 | 恢复到 |
|---|---|
| `episode` | `show-detail`（重新拉取该影片选集） |
| `vod-detail` | `show-detail`（直接跳到影片详情） |
| `vod-list` | `category-page`（恢复到对应分类第 N 页） |
| `category` | `category-page p.1` 或重新选择分类列表 |
| nil | 完整 source → category 流程 |

### C-g 语义

| 层级 | C-g 后 v-cursor | 下次按 v |
|---|---|---|
| 分类列表 | 不变（取消前不写入） | 恢复到上次有效节点 |
| 影片列表 | 保持在当前页 | 重新打开当前页 |
| 播放源 / 选集 | 保持在 vod-detail | 重新打开该影片详情 |

## 各级菜单「返回上一级」（已实现）

常量 `freebox-ui--back-label = ".. (返回上一级)"`，始终排在列表最顶部。

| 菜单 | 选择「返回」后跳转 |
|---|---|
| 分类列表 | 重新选择源 |
| 影片列表 | 重新选择分类 |
| 播放源选择 | 重新进入分类浏览 |
| 选集列表 | 重新调用 select-episode（重选播放源） |

## 最近提交记录

| 提交 | 说明 |
|------|------|
| `73db131` | feat: 各菜单加入「返回上一级」，静默处理 C-g |
| `0b7f3b1` | fix: 修复 vod-detail/episode 节点未记忆问题 |
| `540435e` | fix: 修复 node-level 类型不匹配错误 |
| `a28a3b7` | feat: 实现 v-cursor 节点记忆与恢复机制 |

## 待规划方向（下次继续）

- [ ] **搜索结果页加入「返回上一级」**
  - 当前搜索结果菜单 C-g 静默，但没有显式返回按钮
  - 返回目标：重新调用 `freebox-ui--do-search`（重新输入关键词）

- [ ] **历史浏览记录入口**
  - `freebox-persist-add-history` 已有 clients/sources/categories 三类历史数据
  - 可增加 `H` 键入口，直接从历史记录跳转到某个分类/源
  - 需要在 `freebox-commands.el` 的 hydra 菜单中添加绑定

- [ ] **v 菜单状态行显示当前记忆位置**
  - 在 `freebox-ui-show-current-state` 里追加 v-cursor 节点信息
  - 例如：`[客户端] [源] > 分类名 p.3`
  - 让用户在按 v 前就能看到将要恢复的位置
