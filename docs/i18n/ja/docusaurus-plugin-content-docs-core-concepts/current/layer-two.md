---
sidebar_position: 2
---

# レイヤー2ソリューション

> レイヤー2とはどういう意味ですか？レイヤー2ソリューションにはどのような種類がありますか？

ブロックチェーン業界では、既存のプロトコル（レイヤー1）をオーバーレイして、基盤となるプロトコルに対して追加機能または性能上の利点を提供するソリューションを指す場合、**レイヤー2**ソリューションと呼んでいます。本質的には、それらは他のアプリケーションを可能にするためにプロトコルの上に構築された汎用的な目的のソリューションで、レイヤ1プロトコルと異なるトレードオフを持つ分散型アプリケーションを開発するためのフレームワークを提供します。

## ステートチャネル

Hydra Headプロトコルはレイヤー2のソリューションで、**ステートチャネル**に属し、それ自体が**ペイメントチャネル**の子孫になります。ペイメントチャネルは、2つ以上の当事者が、基礎となるブロックチェーンにすべてのトランザクションをコミットすることなく、特定のオフチェーンプロトコルに従って資金を交換することを可能にします。これは歴史的に、パーミッションレス台帳のスケーラビリティの問題に対する答えとして生まれた最初のレイヤー2ソリューションの一種です（その結果、最も研究されているソリューションの一種でもあります）。

ステートチャネルは、従来の決済チャネルの概念を拡張し、オフチェーンチャネル上でスマートコントラクトをサポートします。このようなセットアップでは、1つまたは複数のパーティはもはや純粋な取引支払いに制限されることなく、オフチェーンで複雑なロジックを扱う本格的なスクリプト検証を実行し、後で結果をレイヤー1にコミットするだけでよいのです。

#### 事例

- Lightning (Bitcoin);
- Perun (Ethereum, Polkadot, Cosmos);
- Sprites (Ethereum);
- 私達のお気に入りである: **Hydra: Head** (Cardano)!

## サイドチェーン

**サイドチェーン** を使用すると、独自のコンセンサスルールのセットを使用して、アセットをレイヤー1プロトコルから新しいチェーンに転送できます。通常、サイドチェーンは、より単純でより効率的なコンセンサスメカニズムを提供します。これにより、スケーラビリティが向上するか、レイヤー1で採用するのが難しい新機能の実装が容易になります。サイドチェーンは通常、ルートに少数のアクターまたは委員会のみが関与しています。

ただし、サイドチェーンは「適切なチェーン」であり、ブロックはバリデーターによって生成され、通常はスマートコントラクト機能を備えています。したがって、ステートチャネルとは異なり、データの可用性と、チェーンの検証と観察に参加する方法を提供します（ステートチャネルでは、チャネルの参加者だけが、チャネルで何が起こっているかについて信頼できるビューを持っています。 ）。サイドチェーンに入るには、通常、レイヤー1でアセットを焼き付けるかロックして、サイドチェーンネットワークで同等のアセットを受け取ります。

#### 事例

- Liquid Network (Bitcoin);
- RSK (Bitcoin);
- Polygon (Ethereum);
- Milkomeda (Cardano).

## ロールアップ

レイヤー2ソリューションのもう1つの主要なタイプは、ロールアップです。これらは、トランザクションの実行をオフチェーンに移動して、レイヤー1での実行の表現をはるかにコンパクトに保つ​​方法を提供します。ロールアップは通常、検証可能なブレッドクラムを定期的に残しながら、高可用性と高い計算機能をオフチェーンで提供する中央アクターによって駆動されます。オンチェーン（ロールアップ）。

一般に、ロールアップには、楽観的またはゼロ知識の2つのフレーバーがあります。前者では、ロールアップは楽観的にチェーンに投稿され、検証は独立したバリデーターによって事後的に行われます。意見の相違がある場合、紛争はチェーン上で解決され、ロールアップ発行者は経済的影響に耐えます。ゼロ知識アプローチでは、実行の簡潔な証明がオフチェーンで計算され、ロールアップと一緒に公開され、オンチェーンバリデーターによって制御されます（したがって、ロールアップの正当な実行が強制されます）

#### 事例

- Arbitrum (Ethereum);
- Optimism (Ethereum);
- Hermez (Ethereum);
- ZKSync (Ethereum).
